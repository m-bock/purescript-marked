module Marked where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Variant (Variant)
import Data.Variant as V
import Data.Variant.Encodings.Flat (VariantEncFlat)
import Data.Variant.Encodings.Flat as VF
import Data.Variant.Encodings.Nested (VariantEncNested)
import Data.Variant.Encodings.Nested as VN
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Union (UndefinedOr, fromOneOf)

type ParseError = String

data Markdown
  = MdSpace
  | MdCode { text :: String, lang :: Maybe String }
  | MdHeading { depth :: Int } (Array Markdown)
  | MdTable
      { align :: Maybe AlignTable
      , header :: Array TableCell
      , rows :: Array (Array TableCell)
      }
  | MdHr
  | MdBlockquote { tokens :: Array Markdown }
  | MdList { items :: Array ListItem }

derive instance Generic Markdown _
derive instance Generic TableCell _
derive instance Generic AlignTable _
derive instance Generic ListItem _

derive instance Eq Markdown
derive instance Eq TableCell
derive instance Eq AlignTable
derive instance Eq ListItem

instance Show Markdown where
  show x = genericShow x

instance Show TableCell where
  show x = genericShow x

instance Show AlignTable where
  show = genericShow

instance Show ListItem where
  show = genericShow

data ListItem = ListItem

data TableCell = TableCell { text :: String, tokens :: Array Markdown }

data AlignTable = AlignCenter | AlignLeft | AlignRight

parse :: String -> Either ParseError (Array Markdown)
parse = parseImpl >>> f >>> map (map h)

type T =
  ( space :: {}
  , code :: { lang :: UndefinedOr String, text :: String }
  )

type MarkdownImpl = VariantEncFlat "type" T

type EitherV a b =
  VariantEncNested "type" "value"
    ( success :: b
    , failure :: a
    )

f :: forall a b. EitherV a b -> Either a b
f = (VN.variantFromVariantEnc) >>>
  ( V.case_ # V.onMatch
      { success: Right
      , failure: Left
      }
  )

h :: VariantEncFlat "type" T -> Markdown
h = (VF.variantFromVariantEnc :: _ -> Variant T) >>>
  ( V.case_ # V.onMatch
      { space: \_ -> MdSpace
      , code: \{ text, lang } -> MdCode { text, lang: fromOneOf lang }
      }
  )

foreign import parseImpl :: String -> EitherV String (Array MarkdownImpl)
