module Test.MarkedSpec where

import Prelude

import Data.Either (Either(..))
import Data.Map (fromFoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Marked (Token(..))
import Marked as ME
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Marked" $ do
  describe "lexer" $ do
    it "Document1" do
      ME.lexer doc1
        `shouldEqual`
          ( Right
              { links:
                  ( fromFoldable
                      [ (Tuple "google-link" { href: (Just "https://www.google.com"), title: Nothing })
                      ]
                  )
              , tokens:
                  [ TokSpace
                  , (TokHeading { depth: 1, tokens: [ (TokText { text: "Markdown Sample" }) ] })
                  , (TokHeading { depth: 2, tokens: [ (TokText { text: "Headers" }) ] })
                  , (TokHeading { depth: 3, tokens: [ (TokText { text: "Subheaders" }) ] })
                  , (TokParagraph { tokens: [ (TokText { text: "This is a paragraph with some " }), (TokStrong {}), (TokText { text: " and " }), (TokEm {}), (TokText { text: ". Also, there&#39;s inline " }), (TokCodespan {}), (TokText { text: "." }) ] })
                  , TokSpace
                  , (TokList {})
                  , TokSpace
                  , (TokList {})
                  , TokSpace
                  , (TokBlockquote {})
                  , (TokParagraph { tokens: [ (TokLink {}) ] })
                  , TokSpace
                  , (TokParagraph { tokens: [ (TokLink {}) ] })
                  , TokSpace
                  , (TokParagraph { tokens: [ (TokImage {}) ] })
                  , TokSpace
                  , (TokHr {})
                  , (TokParagraph { tokens: [ (TokText { text: "Horizontal rule" }) ] })
                  , TokSpace
                  , (TokParagraph { tokens: [ (TokCodespan {}), (TokText { text: ":" }) ] })
                  , TokSpace
                  , (TokCode { lang: (Just "python"), text: "def hello_world():\n    print(\"Hello, world!\")" })
                  , TokSpace
                  , (TokParagraph { tokens: [ (TokText { text: "Tables:" }) ] })
                  , TokSpace
                  , (TokTable {})
                  , (TokParagraph { tokens: [ (TokText { text: "Checkboxes:" }) ] })
                  , TokSpace
                  , (TokList {})
                  , TokSpace
                  , (TokParagraph { tokens: [ (TokText { text: "Footnote[^1] and inline footnote^[Inline Footnote]." }) ] })
                  , TokSpace
                  , (TokParagraph { tokens: [ (TokText { text: "[^1]: This is a footnote." }) ] })
                  , TokSpace
                  , (TokParagraph { tokens: [ (TokText { text: "Mathematics: " }), (TokEscape {}), (TokText { text: "E=mc^2" }), (TokEscape {}) ] })
                  , TokSpace
                  , (TokHr {})
                  , (TokParagraph { tokens: [ (TokText { text: "End of the Markdown sample." }) ] })
                  , TokSpace
                  ]
              }
          )

doc1 :: String
doc1 =
  """
# Markdown Sample

## Headers

### Subheaders

This is a paragraph with some **bold text** and *italic text*. Also, there's inline `code`.

- Unordered list item 1
- Unordered list item 2
  - Subitem 1
  - Subitem 2

1. Ordered list item 1
2. Ordered list item 2
   1. Subitem A
   2. Subitem B

> This is a blockquote.

[Link to Google](https://www.google.com)

[Link to Google][google-link]

![Image Alt Text](https://example.com/image.jpg)

---

Horizontal rule

`Code block`:

```python
def hello_world():
    print("Hello, world!")
```

Tables:

| Header 1 | Header 2 |
|----------|----------|
| Cell 1   | Cell 2   |
| Cell 3   | Cell 4   |

Checkboxes:

- [x] Task 1
- [ ] Task 2

Footnote[^1] and inline footnote^[Inline Footnote].

[^1]: This is a footnote.

Mathematics: \(E=mc^2\)

[google-link]: https://www.google.com

---


End of the Markdown sample.

"""