-- | A module to convert `SvgNode` to halogen `HTML`. Normally you should only
-- | need the `icon` function. See [demo](https://rnons.github.io/purescript-svg-parser-halogen) for an example.
module Svg.Renderer.Halogen
  ( svgElementToHtml
  , svgElementToHtmlWithAttrs
  , svgNodeToHtml
  , parse
  , icon
  ) where

import Prelude
import Data.Bifunctor (lmap)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Halogen.HTML (HTML, IProp, Namespace(Namespace), ElemName(ElemName),
                     AttrName(AttrName), span, text, elementNS, attr)
import Svg.Parser (SvgNode(..), Element, SvgAttribute(..), parseToSvgNode)

ns :: Namespace
ns = Namespace "http://www.w3.org/2000/svg"

svgAttributeToProp :: forall r i. SvgAttribute -> IProp r i
svgAttributeToProp (SvgAttribute k v) = attr (AttrName k) v

-- | Convert `Element` to `HTML`.
svgElementToHtml :: forall p i. Element -> HTML p i
svgElementToHtml ele = svgElementToHtmlWithAttrs ele []

-- | Similar to `svgElementToHtml`, but you can add additional attributes to it.
-- | Useful to apply className or bind event listeners.
svgElementToHtmlWithAttrs :: forall p r i. Element -> Array (IProp r i) -> HTML p i
svgElementToHtmlWithAttrs ele newAttrs =
  elementNS ns (ElemName ele.name) (attrs <> newAttrs) children
  where
    attrs = fromFoldable $ svgAttributeToProp <$> ele.attributes
    children = fromFoldable $ svgNodeToHtml <$> ele.children

-- | Convert `SvgNode` to `HTML`.
svgNodeToHtml :: forall p i. SvgNode -> HTML p i
svgNodeToHtml (SvgElement element) = svgElementToHtml element
svgNodeToHtml (SvgText str) = text str
svgNodeToHtml (SvgComment _str) = text ""

-- | If you don't want to deal with the `SvgNode` type, you can use this function
-- | to parse a `String` as `HTML` directly.
parse :: forall p i. String -> Either String (HTML p i)
parse input =
  lmap show $ svgNodeToHtml <$> parseToSvgNode input

-- | This helper function should be enough for the use case of inline SVG icons.
-- | It takes an SVG source `String` and an array of `IProp`. The output is an
-- | inlined SVG icon with additional className, style or event listeners.
icon :: forall p r i. String -> Array (IProp r i) -> HTML p i
icon input attrs =
  case parseToSvgNode input of
    Right (SvgElement element) -> svgElementToHtmlWithAttrs element attrs
    _ ->
      span [ attr (AttrName "style") "color: red;" ]
      [ text "!SVG Parse Failed!" ]
