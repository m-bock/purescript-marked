-- | PureScript idiomatic wrapper around bindings to marked.js
module Marked
  ( Blockquote
  , Br
  , Code
  , CodeBlockStyle(..)
  , Codespan
  , Def
  , Del
  , Em
  , Escape
  , Heading
  , Hr
  , Html
  , Image
  , LexerError
  , Link
  , LinkRef
  , List
  , ListItem
  , Paragraph
  , Strong
  , Table
  , TableAlign(..)
  , TableCell
  , Tag
  , Text
  , Token(..)
  , lexer
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable as Nullable
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Data.Variant as V
import Data.Variant.Encodings.Flat as VF
import Foreign.Object as Object
import LabeledData.VariantLike.Class as LD
import Literals.Null (Null)
import Marked.Bindings as Bin
import TsBridge (toRecord)
import TsBridge.Types.Intersection as TsIntersection
import TsBridge.Types.Lit (Lit)
import TsBridge.Types.RecordUnion (RecordUnion(..))
import Untagged.Union (uorToMaybe)
import Untagged.Union as UntaggedUnion

-------------------------------------------------------------------------------
--- Types
-------------------------------------------------------------------------------

type LexerError = String

data Token
  = TokSpace
  | TokCode Code
  | TokHeading Heading
  | TokTable Table
  | TokHr Hr
  | TokBlockquote Blockquote
  | TokList List
  | TokListItem ListItem
  | TokParagraph Paragraph
  | TokHtml Html
  | TokText Text
  | TokTag Tag
  | TokDef Def
  | TokEscape Escape
  | TokLink Link
  | TokImage Image
  | TokStrong Strong
  | TokEm Em
  | TokCodespan Codespan
  | TokBr Br
  | TokDel Del

type Code =
  { raw :: String
  , codeBlockStyle :: CodeBlockStyle
  , text :: String
  , lang :: Maybe String
  }

data CodeBlockStyle
  = Indented
  | Fenced

type Heading =
  { raw :: String
  , depth :: Int
  , text :: String
  , tokens :: Array Token
  }

type Table =
  { raw :: String
  , align :: Array TableAlign
  , header :: Array TableCell
  , rows :: Array (Array TableCell)
  }

data TableAlign = AlignCenter | AlignLeft | AlignRight

type ListItem =
  { raw :: String
  , task :: Boolean
  , checked :: Boolean
  , loose :: Boolean
  , text :: String
  , tokens :: Array Token
  }

type Paragraph =
  { raw :: String
  , pre :: Boolean
  , text :: String
  , tokens :: Array Token
  }

type Html =
  { raw :: String
  , pre :: Boolean
  , text :: String
  }

type Text =
  { raw :: String
  , text :: String
  , tokens :: Maybe (Array Token)
  }

type Tag =
  { raw :: String
  , inLink :: Boolean
  , inRawBlock :: Boolean
  , text :: String
  }

type Def =
  { raw :: String
  , tag :: String
  , href :: String
  , title :: String
  }

type Escape =
  { raw :: String
  , text :: String
  }

type Link =
  { raw :: String
  , href :: String
  , title :: Maybe String
  , text :: String
  , tokens :: Array Token
  }

type Image =
  { raw :: String
  , href :: String
  , title :: Maybe String
  , text :: String
  }

type Strong =
  { raw :: String
  , text :: String
  , tokens :: Array Token
  }

type Em =
  { raw :: String
  , text :: String
  , tokens :: Array Token
  }

type Codespan =
  { raw :: String
  , text :: String
  }

type Br =
  { raw :: String }

type Del =
  { raw :: String
  , text :: String
  , tokens :: Array Token
  }

type TableCell = { text :: String, tokens :: Array Token }

type Hr =
  { raw :: String
  }

type Blockquote =
  { raw :: String
  , text :: String
  , tokens :: Array Token
  }

type List =
  { raw :: String
  , ordered :: Boolean
  , start :: Maybe Int
  , loose :: Boolean
  , items :: Array ListItem
  }

type LinkRef =
  { href :: Maybe String
  , title :: Maybe String
  }

-------------------------------------------------------------------------------

lexer :: String -> Either LexerError { tokens :: Array Token, links :: Map String LinkRef }
lexer str = do
  val <- LD.fromVariant $ Bin.lexer str
  let (tokens /\ { links }) = TsIntersection.toTuple val
  pure
    { tokens: map tokenFromImpl tokens
    , links:
        links
          # (Object.toUnfoldable :: _ -> Array _)
          # map (map linkRefFromImpl)
          # Map.fromFoldable
    }

-------------------------------------------------------------------------------

tokenFromImpl :: Bin.Token -> Token
tokenFromImpl (Bin.Token tok) =
  matchVariant $ VF.normalizeEncodingFlat tok
  where
  matchVariant =
    V.case_ # V.onMatch
      { space: \_ ->
          TokSpace

      , code: toRecord >>> \{ raw, codeBlockStyle, text, lang } ->
          TokCode
            { raw
            , codeBlockStyle: case codeBlockStyle of
                Just val | Just _ <- uorToMaybe val -> Indented
                _ -> Fenced
            , text
            , lang: lang >>= uorToMaybe
            }

      , heading: \{ raw, depth, text, tokens } ->
          TokHeading
            { raw
            , depth: fromMaybe 0 $ Int.fromNumber depth
            , text
            , tokens: map tokenFromImpl tokens
            }

      , table: \{ raw, align, header, rows } ->
          TokTable
            { raw
            , align: map alignFromImpl align
            , header: map tableCellFromImpl header
            , rows: map (map tableCellFromImpl) rows
            }

      , hr: \r ->
          TokHr r

      , blockquote: \{ raw, text, tokens } ->
          TokBlockquote
            { raw
            , text
            , tokens: map tokenFromImpl tokens
            }

      , list: \{ raw, ordered, start, loose, items } ->
          TokList
            { raw
            , ordered
            , start: case UntaggedUnion.toEither1 start of
                Left (n :: Number) -> Just $ fromMaybe 0 $ Int.fromNumber n
                Right (_ :: Lit "" String) -> Nothing
            , loose
            , items: map listItemFromImpl items
            }

      , list_item: \r ->
          TokListItem $ listItemFromImpl r

      , paragraph: toRecord >>> \{ raw, pre, text, tokens } ->
          TokParagraph
            { raw
            , pre: fromMaybe false do
                u <- pre
                uorToMaybe u
            , text
            , tokens: map tokenFromImpl tokens
            }

      , html: \(RecordUnion u) -> case UntaggedUnion.toEither1 u of
          Left { raw, inLink, inRawBlock, text } ->
            TokTag { raw, inLink, inRawBlock, text }
          Right { text, pre, raw } ->
            TokHtml { text, pre, raw }

      , text: \(RecordUnion u) -> case UntaggedUnion.toEither1 u of
          Left { raw, inLink, inRawBlock, text } ->
            TokTag { raw, inLink, inRawBlock, text }
          Right r -> toRecord r # \{ raw, text, tokens } ->
            TokText
              { raw
              , text
              , tokens: do
                  u' <- tokens
                  arr <- uorToMaybe u'
                  Just $ map tokenFromImpl arr
              }

      , def: \r ->
          TokDef r

      , escape: \r ->
          TokEscape r

      , link: \r ->
          TokLink $ linkFromImpl r

      , image: \{ raw, href, title, text } ->
          TokImage
            { raw
            , href
            , title: Nullable.toMaybe title
            , text
            }

      , strong: \{ raw, text, tokens } ->
          TokStrong
            { raw
            , text
            , tokens: map tokenFromImpl tokens
            }

      , em: \{ raw, text, tokens } ->
          TokEm
            { raw
            , text
            , tokens: map tokenFromImpl tokens
            }

      , codespan: \r ->
          TokCodespan r

      , br: \r ->
          TokBr r

      , del: \{ raw, text, tokens } ->
          TokDel
            { raw
            , text
            , tokens: map tokenFromImpl tokens
            }
      }

alignFromImpl :: Bin.Align -> TableAlign
alignFromImpl al1 =
  case UntaggedUnion.toEither1 al1 of
    Left (_ :: Lit "center" String) -> AlignCenter
    Right al2 -> case UntaggedUnion.toEither1 al2 of
      Left (_ :: Lit "left" String) -> AlignLeft
      Right al3 -> case UntaggedUnion.toEither1 al3 of
        Left (_ :: Lit "right" String) -> AlignRight
        Right (_ :: Null) -> AlignLeft

tableCellFromImpl :: Bin.TableCell -> TableCell
tableCellFromImpl { text, tokens } =
  { text
  , tokens: map tokenFromImpl tokens
  }

linkFromImpl :: Bin.Link -> Link
linkFromImpl { raw, href, title, text, tokens } =
  { raw
  , href
  , title: Nullable.toMaybe title
  , text
  , tokens: map tokenFromImpl tokens
  }

linkRefFromImpl :: Bin.LinkRef -> LinkRef
linkRefFromImpl { href, title } =
  { href: Nullable.toMaybe href
  , title: Nullable.toMaybe title
  }

listItemFromImpl :: Bin.ListItem -> ListItem
listItemFromImpl = toRecord >>> \{ raw, task, checked, loose, text, tokens } ->
  { raw
  , task
  , checked: fromMaybe false do
      u <- checked
      uorToMaybe u
  , loose
  , text
  , tokens: map tokenFromImpl tokens
  }

-------------------------------------------------------------------------------
--- Instances
-------------------------------------------------------------------------------

derive instance Generic Token _
derive instance Generic TableAlign _
derive instance Generic CodeBlockStyle _

derive instance Eq Token
derive instance Eq TableAlign
derive instance Eq CodeBlockStyle

instance Show Token where
  show x = genericShow x

instance Show TableAlign where
  show = genericShow

instance Show CodeBlockStyle where
  show = genericShow
