# purescript-svg-parser-halogen

<a href="https://pursuit.purescript.org/packages/purescript-svg-parser-halogen">
  <img src="https://pursuit.purescript.org/packages/purescript-svg-parser-halogen/badge"
       alt="purescript-svg-parser-halogen on Pursuit">
  </img>
</a>

A library to inline SVG source string into halogen views.

You might be interested in [svgen](https://github.com/nonbili/svgen), a CLI tool to generate an icons module from svg files.

See [Demo](https://rnons.github.io/purescript-svg-parser-halogen) for an example.

## How to use

```purescript
import Svg.Renderer.Halogen (icon)

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
