module TsBridge.Main where

import Prelude

import DTS as DTS
import Data.Either (Either)
import Effect (Effect)
import Marked.Bindings as Marked.Bindings
import TsBridge as TSB
import TsBridge as TsBridge

myTsProgram :: Either TSB.AppError DTS.TsProgram
myTsProgram =
  TSB.tsProgram
    [ Marked.Bindings.tsModules
    ]

main :: Effect Unit
main = TsBridge.mkTypeGenCli myTsProgram
