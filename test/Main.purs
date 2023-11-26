module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.MarkedSpec as Test.MarkedSpec

main :: Effect Unit
main = launchAff_ do
  runSpec [ consoleReporter ] do
    Test.MarkedSpec.spec