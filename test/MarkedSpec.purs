module Test.MarkedSpec where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Marked as ME
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Marked" $ do
  describe "lexer" $ do
    -- it "works" $ do
    --   ME.parse " " `shouldEqual` (Right [ ME.MdSpace ])
    it "Code" do
      ME.lexer "```\nfoo\n```"
        `shouldEqual`
          (Right [ ME.TokCode { text: "foo", lang: Just "" } ])

    it "Heading" do
      ME.lexer "#"
        `shouldEqual`
          (Right [ ME.TokHeading { depth: 1, tokens: [] } ])
      ME.lexer "##"
        `shouldEqual`
          (Right [ ME.TokHeading { depth: 2, tokens: [] } ])

    it "Paragraph" do
      ME.lexer "hello"
        `shouldEqual`
          (Right [ (ME.TokParagraph { tokens: [ (ME.TokText { text: "hello" }) ] }) ])

    it "Document" do
      ME.lexer doc1
        `shouldEqual`
          ( Right
              [ ME.TokSpace
              , ( ME.TokHeading
                    { depth: 1
                    , tokens:
                        [ (ME.TokText { text: "Heading1" })
                        ]
                    }
                )
              , ( ME.TokParagraph
                    { tokens:
                        [ (ME.TokText { text: "Hello!" })
                        ]
                    }
                )
              , ME.TokSpace
              , ( ME.TokHeading
                    { depth: 2
                    , tokens:
                        [ (ME.TokText { text: "Heading2" })
                        ]
                    }
                )
              , ( ME.TokParagraph
                    { tokens:
                        [ (ME.TokText { text: "Hellooo!" })
                        ]
                    }
                )
              ]
          )

doc1 :: String
doc1 =
  """
# Heading1

Hello!

## Heading2

Hellooo!
"""