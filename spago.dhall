{ name = "marked"
, dependencies =
  [ "console"
  , "dts"
  , "effect"
  , "either"
  , "maybe"
  , "newtype"
  , "nullable"
  , "prelude"
  , "unsafe-coerce"
  , "untagged-union"
  , "variant"
  , "variant-encodings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
