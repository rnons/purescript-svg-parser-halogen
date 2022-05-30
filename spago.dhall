{ name = "svg-parser-halogen"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "either"
  , "halogen"
  , "prelude"
  , "svg-parser"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
