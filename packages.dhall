let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.8-20230517/packages.dhall
        sha256:8b94a0cd7f86589a6bd06d48cb9a61d69b66a94b668657b2f10c8b14c16e028c

in  upstream

  with variant-encodings =
      { dependencies =
          [ "prelude", "unsafe-coerce", "variant" ]
      , repo =
          "https://github.com/thought2/purescript-variant-encodings.git"
      , version =
          "main"
      }


  with ts-bridge =
      { dependencies =
          [ "aff"
          , "aff-promise"
          , "arrays"
          , "console"
          , "dts"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "node-fs"
          , "node-fs-aff"
          , "node-path"
          , "node-process"
          , "nullable"
          , "optparse"
          , "ordered-collections"
          , "ordered-set"
          , "prelude"
          , "record"
          , "safe-coerce"
          , "strings"
          , "transformers"
          , "tuples"
          , "untagged-union"
          , "variant"
          , "variant-encodings"
          ]
      , repo =
          "https://github.com/thought2/purescript-ts-bridge.git"
      , version =
          "main"
      }