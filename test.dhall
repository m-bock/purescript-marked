let conf = ./spago.dhall
in conf // {
  sources = conf.sources # [ "test/**/*.purs", "ts-bridge/**/*.purs" ],
  dependencies = conf.dependencies # 
    [ "spec-discovery"
    , "aff"
    , "spec"
    , "ts-bridge"
    ]
}