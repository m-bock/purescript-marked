module Marked where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (un)
import Data.Show.Generic (genericShow)
import Data.Variant as V
import Data.Variant.Encodings.Flat as VF
import LabeledData.VariantLike.Class as LD
import Marked.Bindings as Bin
import TsBridge (toRecord)
import TsBridge as ITS
import Untagged.Union (uorToMaybe)

-------------------------------------------------------------------------------
--- Types
-------------------------------------------------------------------------------

type LexerError = String

data Token
  = TokSpace
  | TokCode { text :: String, lang :: Maybe String }
  | TokHeading { depth :: Int, tokens :: Array Token }
  | TokTable {}
  | TokHr {}
  | TokBlockquote {}
  | TokList {}
  | TokListItem {}
  | TokParagraph { tokens :: Array Token }
  | TokHtml {}
  | TokText { text :: String }
  | TokDef {}
  | TokEscape {}
  | TokTag {}
  | TokImage {}
  | TokLink {}
  | TokStrong {}
  | TokEm {}
  | TokCodespan {}
  | TokBr {}
  | TokDel {}

data ListItem = ListItem

data TableCell = TableCell { text :: String, tokens :: Array Token }

data AlignTable = AlignCenter | AlignLeft | AlignRight

-------------------------------------------------------------------------------

lexer :: String -> Either LexerError (Array Token)
lexer = Bin.lexer >>> LD.fromVariant >>> map (ITS.fst >>> map (tokenFromImpl))

-------------------------------------------------------------------------------

tokenFromImpl :: Bin.Token -> Token
tokenFromImpl =
  un Bin.Token
    >>> VF.normalizeEncodingFlat
    >>>
      ( V.case_ # V.onMatch
          { space: \_ ->
              TokSpace
          , code: toRecord >>> \{ text, lang } ->
              TokCode
                { text
                , lang: lang >>= uorToMaybe
                }
          , heading: \{ depth, tokens } ->
              TokHeading
                { depth: fromMaybe 0 $ Int.fromNumber depth
                , tokens: map tokenFromImpl tokens
                }
          , table: \_ ->
              TokTable {}
          , hr: \_ ->
              TokHr {}
          , blockquote: \_ ->
              TokBlockquote {}
          , list: \_ ->
              TokList {}
          , list_item: \_ ->
              TokListItem {}
          , paragraph: \{ tokens } ->
              TokParagraph { tokens: map tokenFromImpl tokens }
          , html: \_ ->
              TokHtml {}
          , text: \{ text } ->
              TokText { text }
          , def: \_ ->
              TokDef {}
          , escape: \_ ->
              TokEscape {}
          , tag: \_ ->
              TokTag {}
          , link: \_ ->
              TokLink {}
          , image: \_ ->
              TokImage {}
          , strong: \_ ->
              TokStrong {}
          , em: \_ ->
              TokEm {}
          , codespan: \_ ->
              TokCodespan {}
          , br: \_ ->
              TokBr {}
          , del: \_ ->
              TokDel {}
          }
      )

-------------------------------------------------------------------------------
--- Instances
-------------------------------------------------------------------------------

derive instance Generic Token _
derive instance Generic TableCell _
derive instance Generic AlignTable _
derive instance Generic ListItem _

derive instance Eq Token
derive instance Eq TableCell
derive instance Eq AlignTable
derive instance Eq ListItem

instance Show Token where
  show x = genericShow x

instance Show TableCell where
  show x = genericShow x

instance Show AlignTable where
  show = genericShow

instance Show ListItem where
  show = genericShow

