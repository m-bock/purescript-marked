module Test.MarkedSpec where

import Prelude

import Data.Either (Either(..))
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Marked (CodeBlockStyle(..), TableAlign(..), Token(..))
import Marked as ME
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Marked" $ do
  describe "lexer" $ do
    it "headings" do
      ME.lexer sampleHeadings
        `shouldEqual`
          ( Right
              { links: (fromFoldable [])
              , tokens:
                  [ TokSpace
                  , ( TokHeading
                        { depth: 1
                        , raw: "# Heading 1\n"
                        , text: "Heading 1"
                        , tokens:
                            [ (TokText { raw: "Heading 1", text: "Heading 1", tokens: Nothing }) ]
                        }
                    )
                  , ( TokHeading
                        { depth: 2
                        , raw: "## Heading 2\n"
                        , text: "Heading 2"
                        , tokens:
                            [ (TokText { raw: "Heading 2", text: "Heading 2", tokens: Nothing }) ]
                        }
                    )
                  , ( TokHeading
                        { depth: 3
                        , raw: "### Heading 3\n"
                        , text: "Heading 3"
                        , tokens:
                            [ (TokText { raw: "Heading 3", text: "Heading 3", tokens: Nothing }) ]
                        }
                    )
                  , ( TokHeading
                        { depth: 4
                        , raw: "#### Heading 4\n"
                        , text: "Heading 4"
                        , tokens:
                            [ (TokText { raw: "Heading 4", text: "Heading 4", tokens: Nothing }) ]
                        }
                    )
                  , ( TokHeading
                        { depth: 5
                        , raw: "##### Heading 5\n"
                        , text: "Heading 5"
                        , tokens:
                            [ (TokText { raw: "Heading 5", text: "Heading 5", tokens: Nothing }) ]
                        }
                    )
                  , ( TokHeading
                        { depth: 6
                        , raw: "###### Heading 6\n"
                        , text: "Heading 6"
                        , tokens:
                            [ (TokText { raw: "Heading 6", text: "Heading 6", tokens: Nothing }) ]
                        }
                    )
                  ]
              }
          )
    it "unordered list" do
      ME.lexer sampleUnorderedList
        `shouldEqual`
          ( Right
              { links: (fromFoldable [])
              , tokens:
                  [ TokSpace
                  , ( TokList
                        { items:
                            [ { checked: false
                              , loose: false
                              , raw: "- Item 1\n"
                              , task: false
                              , text: "Item 1"
                              , tokens:
                                  [ ( TokText
                                        { raw: "Item 1"
                                        , text: "Item 1"
                                        , tokens:
                                            ( Just
                                                [ ( TokText
                                                      { raw: "Item 1"
                                                      , text: "Item 1"
                                                      , tokens: Nothing
                                                      }
                                                  )
                                                ]
                                            )
                                        }
                                    )
                                  ]
                              }
                            , { checked: false
                              , loose: false
                              , raw: "- Item 2\n  - Subitem 2.1\n  - Subitem 2.2\n"
                              , task: false
                              , text: "Item 2\n- Subitem 2.1\n- Subitem 2.2"
                              , tokens:
                                  [ ( TokText
                                        { raw: "Item 2\n"
                                        , text: "Item 2"
                                        , tokens:
                                            ( Just
                                                [ ( TokText
                                                      { raw: "Item 2"
                                                      , text: "Item 2"
                                                      , tokens: Nothing
                                                      }
                                                  )
                                                ]
                                            )
                                        }
                                    )
                                  , ( TokList
                                        { items:
                                            [ { checked: false
                                              , loose: false
                                              , raw: "- Subitem 2.1\n"
                                              , task: false
                                              , text: "Subitem 2.1"
                                              , tokens:
                                                  [ ( TokText
                                                        { raw: "Subitem 2.1"
                                                        , text: "Subitem 2.1"
                                                        , tokens:
                                                            ( Just
                                                                [ ( TokText
                                                                      { raw: "Subitem 2.1"
                                                                      , text: "Subitem 2.1"
                                                                      , tokens: Nothing
                                                                      }
                                                                  )
                                                                ]
                                                            )
                                                        }
                                                    )
                                                  ]
                                              }
                                            , { checked: false
                                              , loose: false
                                              , raw: "- Subitem 2.2"
                                              , task: false
                                              , text: "Subitem 2.2"
                                              , tokens:
                                                  [ ( TokText
                                                        { raw: "Subitem 2.2"
                                                        , text: "Subitem 2.2"
                                                        , tokens:
                                                            ( Just
                                                                [ ( TokText
                                                                      { raw: "Subitem 2.2"
                                                                      , text: "Subitem 2.2"
                                                                      , tokens: Nothing
                                                                      }
                                                                  )
                                                                ]
                                                            )
                                                        }
                                                    )
                                                  ]
                                              }
                                            ]
                                        , loose: false
                                        , ordered: false
                                        , raw: "- Subitem 2.1\n- Subitem 2.2"
                                        , start: Nothing
                                        }
                                    )
                                  ]
                              }
                            , { checked: false
                              , loose: false
                              , raw: "- Item 3"
                              , task: false
                              , text: "Item 3"
                              , tokens:
                                  [ ( TokText
                                        { raw: "Item 3"
                                        , text: "Item 3"
                                        , tokens:
                                            ( Just
                                                [ ( TokText
                                                      { raw: "Item 3"
                                                      , text: "Item 3"
                                                      , tokens: Nothing
                                                      }
                                                  )
                                                ]
                                            )
                                        }
                                    )
                                  ]
                              }
                            ]
                        , loose: false
                        , ordered: false
                        , raw: "- Item 1\n- Item 2\n  - Subitem 2.1\n  - Subitem 2.2\n- Item 3\n"
                        , start: Nothing
                        }
                    )
                  ]
              }
          )

    it "ordered list" do
      ME.lexer sampleOrderedList
        `shouldEqual`
          ( Right
              { links: (fromFoldable [])
              , tokens:
                  [ TokSpace
                  , ( TokList
                        { items:
                            [ { checked: false
                              , loose: false
                              , raw: "1. First Item\n"
                              , task: false
                              , text: "First Item"
                              , tokens:
                                  [ ( TokText
                                        { raw: "First Item"
                                        , text: "First Item"
                                        , tokens:
                                            ( Just
                                                [ ( TokText
                                                      { raw: "First Item"
                                                      , text: "First Item"
                                                      , tokens: Nothing
                                                      }
                                                  )
                                                ]
                                            )
                                        }
                                    )
                                  ]
                              }
                            , { checked: false
                              , loose: false
                              , raw: "2. Second Item\n   1. Subitem 2.1\n   2. Subitem 2.2\n"
                              , task: false
                              , text: "Second Item\n1. Subitem 2.1\n2. Subitem 2.2"
                              , tokens:
                                  [ ( TokText
                                        { raw: "Second Item\n"
                                        , text: "Second Item"
                                        , tokens:
                                            ( Just
                                                [ ( TokText
                                                      { raw: "Second Item"
                                                      , text: "Second Item"
                                                      , tokens: Nothing
                                                      }
                                                  )
                                                ]
                                            )
                                        }
                                    )
                                  , ( TokList
                                        { items:
                                            [ { checked: false
                                              , loose: false
                                              , raw: "1. Subitem 2.1\n"
                                              , task: false
                                              , text: "Subitem 2.1"
                                              , tokens:
                                                  [ ( TokText
                                                        { raw: "Subitem 2.1"
                                                        , text: "Subitem 2.1"
                                                        , tokens:
                                                            ( Just
                                                                [ ( TokText
                                                                      { raw: "Subitem 2.1"
                                                                      , text: "Subitem 2.1"
                                                                      , tokens: Nothing
                                                                      }
                                                                  )
                                                                ]
                                                            )
                                                        }
                                                    )
                                                  ]
                                              }
                                            , { checked: false
                                              , loose: false
                                              , raw: "2. Subitem 2.2"
                                              , task: false
                                              , text: "Subitem 2.2"
                                              , tokens:
                                                  [ ( TokText
                                                        { raw: "Subitem 2.2"
                                                        , text: "Subitem 2.2"
                                                        , tokens:
                                                            ( Just
                                                                [ ( TokText
                                                                      { raw: "Subitem 2.2"
                                                                      , text: "Subitem 2.2"
                                                                      , tokens: Nothing
                                                                      }
                                                                  )
                                                                ]
                                                            )
                                                        }
                                                    )
                                                  ]
                                              }
                                            ]
                                        , loose: false
                                        , ordered: true
                                        , raw: "1. Subitem 2.1\n2. Subitem 2.2"
                                        , start: (Just 1)
                                        }
                                    )
                                  ]
                              }
                            , { checked: false
                              , loose: false
                              , raw: "3. Third Item"
                              , task: false
                              , text: "Third Item"
                              , tokens:
                                  [ ( TokText
                                        { raw: "Third Item"
                                        , text: "Third Item"
                                        , tokens:
                                            ( Just
                                                [ ( TokText
                                                      { raw: "Third Item"
                                                      , text: "Third Item"
                                                      , tokens: Nothing
                                                      }
                                                  )
                                                ]
                                            )
                                        }
                                    )
                                  ]
                              }
                            ]
                        , loose: false
                        , ordered: true
                        , raw:
                            "1. First Item\n2. Second Item\n   1. Subitem 2.1\n   2. Subitem 2.2\n3. Third Item\n"
                        , start: (Just 1)
                        }
                    )
                  ]
              }
          )

    it "links" do
      ME.lexer sampleLinks
        `shouldEqual`
          ( Right
              { links: (fromFoldable [])
              , tokens:
                  [ TokSpace
                  , ( TokParagraph
                        { pre: false
                        , raw:
                            "[Google](https://www.google.com)\n[Markdown Guide](https://www.markdownguide.org)\n"
                        , text:
                            "[Google](https://www.google.com)\n[Markdown Guide](https://www.markdownguide.org)"
                        , tokens:
                            [ ( TokLink
                                  { href: "https://www.google.com"
                                  , raw: "[Google](https://www.google.com)"
                                  , text: "Google"
                                  , title: Nothing
                                  , tokens:
                                      [ (TokText { raw: "Google", text: "Google", tokens: Nothing })
                                      ]
                                  }
                              )
                            , (TokText { raw: "\n", text: "\n", tokens: Nothing })
                            , ( TokLink
                                  { href: "https://www.markdownguide.org"
                                  , raw: "[Markdown Guide](https://www.markdownguide.org)"
                                  , text: "Markdown Guide"
                                  , title: Nothing
                                  , tokens:
                                      [ ( TokText
                                            { raw: "Markdown Guide"
                                            , text: "Markdown Guide"
                                            , tokens: Nothing
                                            }
                                        )
                                      ]
                                  }
                              )
                            ]
                        }
                    )
                  ]
              }
          )

    it "emphasis" do
      ME.lexer sammpleEmphasis
        `shouldEqual`
          ( Right
              { links: (fromFoldable [])
              , tokens:
                  [ TokSpace
                  , ( TokParagraph
                        { pre: false
                        , raw: "*Italic Text*\n**Bold Text**\n"
                        , text: "*Italic Text*\n**Bold Text**"
                        , tokens:
                            [ ( TokEm
                                  { raw: "*Italic Text*"
                                  , text: "Italic Text"
                                  , tokens:
                                      [ ( TokText
                                            { raw: "Italic Text"
                                            , text: "Italic Text"
                                            , tokens: Nothing
                                            }
                                        )
                                      ]
                                  }
                              )
                            , (TokText { raw: "\n", text: "\n", tokens: Nothing })
                            , ( TokStrong
                                  { raw: "**Bold Text**"
                                  , text: "Bold Text"
                                  , tokens:
                                      [ ( TokText
                                            { raw: "Bold Text", text: "Bold Text", tokens: Nothing }
                                        )
                                      ]
                                  }
                              )
                            ]
                        }
                    )
                  ]
              }
          )

    it "code" do
      ME.lexer sampleCode
        `shouldEqual`
          ( Right
              { links: (fromFoldable [])
              , tokens:
                  [ TokSpace
                  , ( TokParagraph
                        { pre: false
                        , raw: "This is `inline code`.\n"
                        , text: "This is `inline code`."
                        , tokens:
                            [ (TokText { raw: "This is ", text: "This is ", tokens: Nothing })
                            , (TokCodespan { raw: "`inline code`", text: "inline code" })
                            , (TokText { raw: ".", text: ".", tokens: Nothing })
                            ]
                        }
                    )
                  ]
              }
          )

    it "code blocks" do
      ME.lexer sampleCodeBlocks
        `shouldEqual`
          ( Right
              { links: (fromFoldable [])
              , tokens:
                  [ TokSpace
                  , ( TokCode
                        { codeBlockStyle: Fenced
                        , lang: (Just "python")
                        , raw: "```python\ndef hello_world():\n    print(\"Hello, World!\")\n```\n"
                        , text: "def hello_world():\n    print(\"Hello, World!\")"
                        }
                    )
                  ]
              }
          )
    it "images" do
      ME.lexer sampleImages
        `shouldEqual`
          ( Right
              { links: (fromFoldable [])
              , tokens:
                  [ TokSpace
                  , ( TokParagraph
                        { pre: false
                        , raw: "![Alt Text](https://example.com/image.jpg)\n"
                        , text: "![Alt Text](https://example.com/image.jpg)"
                        , tokens:
                            [ ( TokImage
                                  { href: "https://example.com/image.jpg"
                                  , raw: "![Alt Text](https://example.com/image.jpg)"
                                  , text: "Alt Text"
                                  , title: Nothing
                                  }
                              )
                            ]
                        }
                    )
                  ]
              }
          )
    it "blockquotes" do
      ME.lexer sampleBlockquotes
        `shouldEqual`
          ( Right
              { links: (fromFoldable [])
              , tokens:
                  [ TokSpace
                  , ( TokBlockquote
                        { raw: "> This is a blockquote.\n> It can span multiple lines.\n"
                        , text: "This is a blockquote.\nIt can span multiple lines.\n"
                        , tokens:
                            [ ( TokParagraph
                                  { pre: false
                                  , raw: "This is a blockquote.\nIt can span multiple lines.\n"
                                  , text: "This is a blockquote.\nIt can span multiple lines."
                                  , tokens:
                                      [ ( TokText
                                            { raw:
                                                "This is a blockquote.\nIt can span multiple lines."
                                            , text:
                                                "This is a blockquote.\nIt can span multiple lines."
                                            , tokens: Nothing
                                            }
                                        )
                                      ]
                                  }
                              )
                            ]
                        }
                    )
                  ]
              }
          )

    it "horizontal rule" do
      ME.lexer sampleHorizontalRule
        `shouldEqual`
          (Right { links: (fromFoldable []), tokens: [ TokSpace, (TokHr { raw: "---\n" }) ] })

    it "tables" do
      ME.lexer sampleTables
        `shouldEqual`
          ( Right
              { links: (fromFoldable [])
              , tokens:
                  [ TokSpace
                  , ( TokTable
                        { align: [ AlignLeft, AlignLeft ]
                        , header:
                            [ { text: "Header 1"
                              , tokens:
                                  [ (TokText { raw: "Header 1", text: "Header 1", tokens: Nothing })
                                  ]
                              }
                            , { text: "Header 2"
                              , tokens:
                                  [ (TokText { raw: "Header 2", text: "Header 2", tokens: Nothing })
                                  ]
                              }
                            ]
                        , raw:
                            "| Header 1 | Header 2 |\n| -------- | -------- |\n| Row 1, Col 1 | Row 1, Col 2 |\n| Row 2, Col 1 | Row 2, Col 2 |\n"
                        , rows:
                            [ [ { text: "Row 1, Col 1"
                                , tokens:
                                    [ ( TokText
                                          { raw: "Row 1, Col 1"
                                          , text: "Row 1, Col 1"
                                          , tokens: Nothing
                                          }
                                      )
                                    ]
                                }
                              , { text: "Row 1, Col 2"
                                , tokens:
                                    [ ( TokText
                                          { raw: "Row 1, Col 2"
                                          , text: "Row 1, Col 2"
                                          , tokens: Nothing
                                          }
                                      )
                                    ]
                                }
                              ]
                            , [ { text: "Row 2, Col 1"
                                , tokens:
                                    [ ( TokText
                                          { raw: "Row 2, Col 1"
                                          , text: "Row 2, Col 1"
                                          , tokens: Nothing
                                          }
                                      )
                                    ]
                                }
                              , { text: "Row 2, Col 2"
                                , tokens:
                                    [ ( TokText
                                          { raw: "Row 2, Col 2"
                                          , text: "Row 2, Col 2"
                                          , tokens: Nothing
                                          }
                                      )
                                    ]
                                }
                              ]
                            ]
                        }
                    )
                  ]
              }
          )

sampleHeadings :: String
sampleHeadings =
  """
# Heading 1
## Heading 2
### Heading 3
#### Heading 4
##### Heading 5
###### Heading 6
"""

sampleUnorderedList :: String
sampleUnorderedList =
  """
- Item 1
- Item 2
  - Subitem 2.1
  - Subitem 2.2
- Item 3
"""

sampleOrderedList :: String
sampleOrderedList =
  """
1. First Item
2. Second Item
   1. Subitem 2.1
   2. Subitem 2.2
3. Third Item
"""

sampleLinks :: String
sampleLinks =
  """
[Google](https://www.google.com)
[Markdown Guide](https://www.markdownguide.org)
"""

sammpleEmphasis :: String
sammpleEmphasis =
  """
*Italic Text*
**Bold Text**
"""

sampleCode :: String
sampleCode =
  """
This is `inline code`.
"""

sampleCodeBlocks :: String
sampleCodeBlocks =
  """
```python
def hello_world():
    print("Hello, World!")
```
"""

sampleImages :: String
sampleImages =
  """
![Alt Text](https://example.com/image.jpg)
"""

sampleBlockquotes :: String
sampleBlockquotes =
  """
> This is a blockquote.
> It can span multiple lines.
"""

sampleHorizontalRule :: String
sampleHorizontalRule =
  """
---
"""

sampleTables :: String
sampleTables =
  """
| Header 1 | Header 2 |
| -------- | -------- |
| Row 1, Col 1 | Row 1, Col 2 |
| Row 2, Col 1 | Row 2, Col 2 |
"""