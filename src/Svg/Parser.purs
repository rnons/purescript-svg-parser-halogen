-- | A module to Parse an SVG document `String` as `SvgNode`.  If you are using
-- | `purescript-halogen`, `Svg.Parser.Halogen` is for you. If you are using
-- | other view libraries, you need to write an adapter to convert `SvgNode` to
-- | the `HTML` type you are using.
module Svg.Parser
  ( SvgNode(..)
  , Element(..)
  , SvgAttribute(..)
  , parseToSvgNode
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.List as List
import Data.String (fromCharArray)
import Text.Parsing.StringParser (Parser, ParseError, runParser, try)
import Text.Parsing.StringParser.Combinators (lookAhead, many, manyTill, option, sepEndBy)
import Text.Parsing.StringParser.String (anyChar, regex, skipSpaces, string, whiteSpace)


-- | A SVG node can be one of the three: SvgElement, SvgText or SvgComment.
data SvgNode
  = SvgElement Element
  | SvgText String
  | SvgComment String

derive instance eqSvgNode :: Eq SvgNode
derive instance genericRepSvgNode :: Generic SvgNode _
instance showSvgNode :: Show SvgNode where show = defer \_ -> genericShow

-- | An Element consists of a tag name, a list of attributes, a list of children nodes.
-- |
-- | <svg xmlns="http://www.w3.org/2000/svg"></svg>
-- |
-- | will be parsed as
-- |
-- | ``` purescript
-- | { name: "svg"
-- | , attributes: [ SvgAttribute "xmlns" "http://www.w3.org/2000/svg" ]
-- | , children: []
-- | }
type Element =
  { name :: String
  , attributes :: List SvgAttribute
  , children :: List SvgNode
  }

-- | An attribute consists of attribute name and value
data SvgAttribute = SvgAttribute String String

derive instance eqSvgAttribute :: Eq SvgAttribute
derive instance genericRepSvgAttribute :: Generic SvgAttribute _
instance showSvgAttribute :: Show SvgAttribute where show = genericShow

mkElement :: String -> List SvgAttribute -> List SvgNode -> Element
mkElement =
  { name: _
  , attributes: _
  , children: _
  }

charListToString :: List Char -> String
charListToString = fromCharArray <<< fromFoldable

attributeParser :: Parser SvgAttribute
attributeParser = do
  k <- regex "[^=>/]+"
  v <- option "" (string "=\"" *> regex "[^\"]*" <* string "\"")
  pure $ SvgAttribute k v

openingParser :: Parser Element
openingParser = do
  void $ string "<"
  tagName <- regex "[^/> ]+"
  attributes <- skipSpaces *> sepEndBy attributeParser whiteSpace
  pure $ mkElement tagName attributes List.Nil

closingOrChildrenParser :: Element -> Parser Element
closingOrChildrenParser element = defer \_ ->
  try (skipSpaces *> string "/>" *> pure element) <|>
  childrenParser
  where
    childrenParser = do
      void $ skipSpaces *> string ">"
      children <- many $ try nodeParser
      void $ skipSpaces *> string ("</" <> element.name <> ">")
      pure $ element { children = children }


elementParser :: Parser SvgNode
elementParser = defer \_ -> do
  skipSpaces
  openingParser >>=
    closingOrChildrenParser >>=
    pure <<< SvgElement


textParser :: Parser SvgNode
textParser = do
  skipSpaces
  SvgText <$> regex "[^<]+"


commentParser :: Parser SvgNode
commentParser = do
  skipSpaces
  comment <- string "<!--" *> manyTill anyChar (string "-->")
  pure $ SvgComment $ charListToString comment


nodeParser :: Parser SvgNode
nodeParser = defer \_ ->
  try textParser <|>
  try commentParser <|>
  elementParser

xmlDeclarationParser :: Parser String
xmlDeclarationParser = do
  skipSpaces
  decl <- string "<?xml" *> manyTill anyChar (string "?>")
  pure $ charListToString decl

beforeSvgParser :: Parser String
beforeSvgParser = do
  charListToString <$> manyTill anyChar (lookAhead $ string "<svg")

-- | Parse an SVG source `String` as `SvgNode`.
-- | You can then use `SvgNode` to construct the `HTML` type you are using.
-- |
-- | NOTE: Everything before `<svg` and after `</svg>` are ignored.
parseToSvgNode :: String -> Either ParseError SvgNode
parseToSvgNode input =
  runParser (option "" (try beforeSvgParser) *> nodeParser) input
