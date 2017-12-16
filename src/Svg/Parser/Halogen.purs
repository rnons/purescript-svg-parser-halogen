module Svg.Parser.Halogen where

import Prelude
import Data.Bifunctor (lmap)
import Data.Array (fromFoldable)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Halogen.HTML (HTML, text)
import Halogen.HTML.Core (Namespace(Namespace), ElemName(ElemName), Prop,
                          AttrName(AttrName), element, attr)
import Svg.Parser (SvgNode(..), Element, SvgAttribute(..), parseToSvgNode)

ns :: Maybe Namespace
ns = Just $ Namespace "http://www.w3.org/2000/svg"

svgAttributeToProp :: forall i. SvgAttribute -> Prop i
svgAttributeToProp (SvgAttribute k v) = attr Nothing (AttrName k) v

svgElementToHtml :: forall p i. Element -> HTML p i
svgElementToHtml ele =
  element ns (ElemName ele.name) attrs children
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
