# purescript-svg-parser-halogen

A library to inline SVG source string into halogen views.

See [Demo](https://rnons.github.io/purescript-svg-parser-halogen) for an example.

## How to use

```purescript
import Svg.Parser.Halogen (icon)

-- | You can use FFI and webpack raw-loader to load external SVG files
code :: String
code = """<svg xmlns="http://www.w3.org/2000/svg" width="14" height="16" viewBox="0 0 14 16"><path fill-rule="evenodd" d="M9.5 3L8 4.5 11.5 8 8 11.5 9.5 13 14 8 9.5 3zm-5 0L0 8l4.5 5L6 11.5 2.5 8 6 4.5 4.5 3z"/></svg>"""

type Icon = forall p r i. Array (IProp r i) -> HTML p i

iconCode :: Icon
iconCode = icon code
```

It's as simple as this, in most cases you only need the `icon` function. You can then use `iconCode` in your `render` function, you can also apply additional className to it.


`Halogen.HTML.Properties.class_` won't work though, you need to use `Halogen.HTML.attr`.

```purescript
import Halogen.HTML as HH

className = HH.attr (HH.AttrName "class")

render state =
  iconCode [ className "icon" ]
```

## How it works

`Svg.Parser` parses SVG source `String` as `SvgNode`. `Svg.Parser.Halogen` converts `SvgNode` to halogen `HTML`. You can also write adapters to convert `SvgNode` to the `HTML` type of other view libraries.

If you want to `Svg.Parser` with other view libraries, I can release it as a separate package, let me know if you are interested.
