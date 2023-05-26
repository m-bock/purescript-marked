module Marked where

import Prelude

import DTS as DTS
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Show.Generic (genericShow)
import Data.Variant (Variant)
import Data.Variant as V
import Data.Variant.Encodings.Flat (VariantEncFlat)
import Data.Variant.Encodings.Flat as VF
import Data.Variant.Encodings.Nested (VariantEncNested)
import Data.Variant.Encodings.Nested as VN
import TsBridge as TSB
import TsBridge.Class (Tok(..))
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Union (UndefinedOr, fromOneOf)

-------------------------------------------------------------------------------
--- Types
-------------------------------------------------------------------------------

type LexerError = String

data Token
  = TokSpace
  | TokCode { text :: String, lang :: Maybe String }
  | TokHeading { depth :: Int, tokens :: Array Token }
  --
  | TokText { text :: String }
  | TokParagraph { tokens :: Array Token }

-- | TokTable
--     { align :: Maybe AlignTable
--     , header :: Array TableCell
--     , rows :: Array (Array TableCell)
--     }
-- | TokHr
-- | TokBlockquote { tokens :: Array Token }
-- | TokList { items :: Array ListItem }

data ListItem = ListItem

data TableCell = TableCell { text :: String, tokens :: Array Token }

data AlignTable = AlignCenter | AlignLeft | AlignRight

-------------------------------------------------------------------------------

lexer :: String -> Either LexerError (Array Token)
lexer = lexerImpl >>> eitherFromImpl >>> map (map tokenFromImpl)

-------------------------------------------------------------------------------

eitherFromImpl :: forall a b. EitherImpl a b -> Either a b
eitherFromImpl = (VN.variantFromVariantEnc) >>>
  ( V.case_ # V.onMatch
      { success: Right
      , failure: Left
      }
  )

tokenFromImpl :: TokenImpl -> Token
tokenFromImpl = un TokenImpl >>> (VF.variantFromVariantEnc :: _ -> Variant TokenRow) >>>
  ( V.case_ # V.onMatch
      { space: \_ -> TokSpace
      , code: \{ text, lang } -> TokCode { text, lang: Nullable.toMaybe lang }
      , heading: \{ depth, tokens } -> TokHeading { depth, tokens: map tokenFromImpl tokens }
      , text: \{ text } -> TokText { text }
      , paragraph: \{ tokens } -> TokParagraph { tokens: map tokenFromImpl tokens }
      }
  )

-------------------------------------------------------------------------------
--- FFI
-------------------------------------------------------------------------------

newtype TokenImpl = TokenImpl (VariantEncFlat "type" TokenRow)

type TokenRow =
  ( space :: {}
  , code :: { lang :: Nullable String, text :: String }
  , heading :: { depth :: Int, tokens :: Array TokenImpl }
  , text :: { text :: String }
  , paragraph :: { tokens :: Array TokenImpl }
  )

type EitherImpl a b =
  VariantEncNested "type" "value"
    ( success :: b
    , failure :: a
    )

foreign import lexerImpl :: String -> EitherImpl String (Array TokenImpl)

-------------------------------------------------------------------------------
--- Instances
-------------------------------------------------------------------------------

derive instance Generic Token _
derive instance Generic TableCell _
derive instance Generic AlignTable _
derive instance Generic ListItem _

derive instance Newtype TokenImpl _

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

-------------------------------------------------------------------------------
--- TsBridge
-------------------------------------------------------------------------------

moduleName :: String
moduleName = "Marked"

tsModules :: Either TSB.AppError (Array DTS.TsModuleFile)
tsModules =
  TSB.tsModuleFile moduleName
    [
    -- TSB.tsTypeAlias "FFI" (Proxy :: _ FFI)
    ]