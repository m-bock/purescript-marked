{ name = "marked"
, dependencies =
  [ "console"
  , "dts"
  , "effect"
  , "either"
  , "integers"
  , "labeled-data"
  , "maybe"
  , "newtype"
  , "nullable"
  , "prelude"
  , "ts-bridge"
  , "unsafe-coerce"
  , "untagged-union"
  , "variant"
  , "variant-encodings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
