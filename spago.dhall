{ name = "marked"
, dependencies =
  [ "console"
  , "effect"
  , "either"
  , "maybe"
  , "prelude"
  , "unsafe-coerce"
  , "untagged-union"
  , "variant"
  , "variant-encodings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
