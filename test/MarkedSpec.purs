module Test.MarkedSpec where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Marked as ME
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Marked" $ do
  describe "parse" $ do
    -- it "works" $ do
    --   ME.parse " " `shouldEqual` (Right [ ME.MdSpace ])
    it "Code" $
      do
        ME.parse "```\nfoo\n```"
          `shouldEqual`
            (Right [ ME.MdCode { text: "foo", lang: Just "" } ])