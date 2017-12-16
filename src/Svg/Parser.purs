module Svg.Parser where

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
import Text.Parsing.StringParser.Combinators (choice, manyTill, option, sepEndBy)
import Text.Parsing.StringParser.String (anyChar, regex, string, whiteSpace, skipSpaces)

data SvgNode
  = SvgElement Element
  | SvgText String
  | SvgComment String

derive instance eqSvgNode :: Eq SvgNode
derive instance genericRepSvgNode :: Generic SvgNode _
instance showSvgNode :: Show SvgNode where show = defer \_ -> genericShow

type Element =
  { name :: String
  , attributes :: List SvgAttribute
  , children :: List SvgNode
  }

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
  _ <- string "<"
  tagName <- regex "[^/> ]+"
  attributes <- whiteSpace *> sepEndBy attributeParser whiteSpace
  pure $ mkElement tagName attributes List.Nil

closingOrChildrenParser :: Element -> Parser Element
closingOrChildrenParser element = defer \_ ->
  choice
    [ whiteSpace *> string "/>" *> pure element
    , childrenParser
    ]
  where
    childrenParser = do
      _ <- whiteSpace *> string ">"
      children <- manyTill nodeParser
                 (whiteSpace <* string ("</" <> element.name <> ">"))
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

parseToSvgNode :: String -> Either ParseError SvgNode
parseToSvgNode input =
  runParser (option "" (try xmlDeclarationParser) *> nodeParser) input
