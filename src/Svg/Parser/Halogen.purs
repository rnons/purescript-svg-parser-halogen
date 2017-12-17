module Svg.Parser.Halogen
  ( svgElementToHtml
  , svgElementToHtmlWithAttrs
  , svgNodeToHtml
  , parse
  , icon
  ) where

import Prelude
import Data.Bifunctor (lmap)
import Data.Array (fromFoldable)
import Data.Either (Either(Right))
import Data.Maybe (Maybe(..))
import Halogen.HTML (HTML, IProp, Namespace(Namespace), ElemName(ElemName),
                     AttrName(AttrName), text, elementNS, attr)
import Svg.Parser (SvgNode(..), Element, SvgAttribute(..), parseToSvgNode)

ns :: Namespace
ns = Namespace "http://www.w3.org/2000/svg"

svgAttributeToProp :: forall r i. SvgAttribute -> IProp r i
svgAttributeToProp (SvgAttribute k v) = attr (AttrName k) v

svgElementToHtml :: forall p i. Element -> HTML p i
svgElementToHtml ele = svgElementToHtmlWithAttrs ele []

svgElementToHtmlWithAttrs :: forall p r i. Element -> Array (IProp r i) -> HTML p i
svgElementToHtmlWithAttrs ele newAttrs =
  elementNS ns (ElemName ele.name) (attrs <> newAttrs) children
  where
    attrs = fromFoldable $ svgAttributeToProp <$> ele.attributes
    children = fromFoldable $ svgNodeToHtml <$> ele.children

svgNodeToHtml :: forall p i. SvgNode -> HTML p i
svgNodeToHtml (SvgElement element) = svgElementToHtml element
svgNodeToHtml (SvgText str) = text str
svgNodeToHtml (SvgComment str) = text ""

parse :: forall p i. String -> Either String (HTML p i)
parse input =
  lmap show $ svgNodeToHtml <$> parseToSvgNode input

icon :: forall p r i. String -> Array (IProp r i) -> HTML p i
icon input attrs =
  case parseToSvgNode input of
    Right (SvgElement element) -> svgElementToHtmlWithAttrs element attrs
    _ -> text ""

