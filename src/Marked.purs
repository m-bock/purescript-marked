module Marked where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Nullable as Nullable
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Variant as V
import Data.Variant.Encodings.Flat as VF
import Foreign.Object as Object
import LabeledData.VariantLike.Class as LD
import Literals.Null (Null)
import Marked.Bindings (StringLit)
import Marked.Bindings as Bin
import TsBridge (toRecord)
import TsBridge.Types.Intersection as TsIntersection
import Untagged.Union (uorToMaybe)
import Untagged.Union as UntaggedUnion

-------------------------------------------------------------------------------
--- Types
-------------------------------------------------------------------------------

type LexerError = String

type Link =
  { href :: String
  , title :: String
  }

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
  | TokText { text :: String }
  | TokDef {}
  | TokEscape {}
  -- | TokTag {}
  | TokImage {}
  | TokLink {}
  | TokStrong {}
  | TokEm {}
  | TokCodespan {}
  | TokBr {}
  | TokDel {}

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
  , block :: Boolean
  , text :: String
  , inLink :: Boolean
  , inRawBlock :: Boolean
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
lexer str =
  let
    result = LD.fromVariant $ Bin.lexer str
  in
    case result of
      Left err -> Left err
      Right val | (Tuple tokens { links }) <- TsIntersection.toTuple val -> Right
        { tokens: map tokenFromImpl tokens
        , links:
            links
              # (Object.toUnfoldable :: _ -> Array _)
              # map (map linkRefFromImpl)
              # Map.fromFoldable
        }

-------------------------------------------------------------------------------

tokenFromImpl :: Bin.Token -> Token
tokenFromImpl =
  un Bin.Token
    >>> VF.normalizeEncodingFlat
    >>>
      ( V.case_ # V.onMatch
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
                    Right (_ :: StringLit "") -> Nothing
                , loose
                , items: map listItemFromImpl items
                }
          , list_item: \r ->
              TokListItem $ listItemFromImpl r
          , paragraph: toRecord >>> \{ raw, pre, text, tokens } ->
              TokParagraph
                { raw
                , pre: case pre of
                    Just u -> case uorToMaybe u of
                      Just bool -> bool
                      Nothing -> false
                    Nothing -> false
                , text
                , tokens: map tokenFromImpl tokens
                }
          , html: toRecord >>> \{ raw, pre, block, text, inLink, inRawBlock } ->
              TokHtml
                { raw
                , pre
                , block
                , text
                , inLink: case inLink of
                    Just bool -> bool
                    Nothing -> false
                , inRawBlock: case inRawBlock of
                    Just bool -> bool
                    Nothing -> false
                }
          -- , text: unsafeCoerce ""
          -- , def: \_ ->
          --     TokDef {}
          -- , escape: \_ ->
          --     TokEscape {}
          -- -- , tag: \_ ->
          -- --     TokTag {}
          -- , link: \_ ->
          --     TokLink {}
          -- , image: \_ ->
          --     TokImage {}
          -- , strong: \_ ->
          --     TokStrong {}
          -- , em: \_ ->
          --     TokEm {}
          -- , codespan: \_ ->
          --     TokCodespan {}
          -- , br: \_ ->
          --     TokBr {}
          -- , del: \_ ->
          --     TokDel {}
          }
      )

alignFromImpl :: Bin.Align -> TableAlign
alignFromImpl al1 =
  case UntaggedUnion.toEither1 al1 of
    Left (_ :: StringLit "center") -> AlignCenter
    Right al2 -> case UntaggedUnion.toEither1 al2 of
      Left (_ :: StringLit "left") -> AlignLeft
      Right al3 -> case UntaggedUnion.toEither1 al3 of
        Left (_ :: StringLit "right") -> AlignRight
        Right (_ :: Null) -> AlignLeft

tableCellFromImpl :: Bin.TableCell -> TableCell
tableCellFromImpl { text, tokens } =
  { text
  , tokens: map tokenFromImpl tokens
  }

linkFromImpl :: Bin.Link -> Link
linkFromImpl { href, title } =
  { href
  , title
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
  , checked: case checked of
      Just u -> case uorToMaybe u of
        Just bool -> bool
        Nothing -> false
      Nothing -> false
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
  show x = genericShow x
