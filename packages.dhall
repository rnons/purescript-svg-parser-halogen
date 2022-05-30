let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220527/packages.dhall
        sha256:15dd8041480502850e4043ea2977ed22d6ab3fc24d565211acde6f8c5152a799

let additions =
      { svg-parser =
        { repo = "https://github.com/rnons/purescript-svg-parser.git"
        , version = "b920559f0f3d7f8134dea17aa7652f07a8ec957e"
        , dependencies =
          [ "arrays"
          , "control"
          , "either"
          , "lists"
          , "prelude"
          , "string-parsers"
          , "strings"
          ]
        }
      }

in  additions // upstream
