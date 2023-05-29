module Marked where

import Prelude

import DTS as DTS
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, un)
import Data.Show.Generic (genericShow)
import Data.Variant as V
import Data.Variant.Encodings.Flat (VariantEncodedFlat)
import Data.Variant.Encodings.Flat as VF
import LabeledData.VariantLike.Class (EitherV)
import LabeledData.VariantLike.Class as LD
import Prim.Boolean (True)
import TsBridge (Mod, TsRecord, toRecord)
import TsBridge as TSB
import TsBridge.Class (class TsBridge, Tok(..))
import Type.Proxy (Proxy(..))
import Untagged.Union (UndefinedOr, uorToMaybe)

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
lexer = lexerImpl >>> LD.fromVariant >>> map (map tokenFromImpl)

-------------------------------------------------------------------------------

tokenFromImpl :: TokenImpl -> Token
tokenFromImpl =
  un TokenImpl
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
--- FFI
-------------------------------------------------------------------------------

newtype TokenImpl = TokenImpl (VariantEncodedFlat "type" TokenRow)

instance TsBridge TokenImpl where
  tsBridge x = TSB.tsBridgeNewtype
    Tok
    { moduleName
    , typeName: "TokenImpl"
    , typeArgs: []
    }
    x

type TokenRow =
  ( space :: {}
  , code ::
      TsRecord
        ( lang :: Mod (optional :: True) (UndefinedOr String)
        , text :: Mod () String
        )
  , heading :: { depth :: Number, tokens :: Array TokenImpl }
  , table :: {}
  , hr :: {}
  , blockquote :: {}
  , list :: {}
  , list_item :: {}
  , paragraph :: { tokens :: Array TokenImpl }
  , html :: {}
  , text :: { text :: String }
  , def :: {}
  , escape :: {}
  , tag :: {}
  , link :: {}
  , image :: {}
  , strong :: {}
  , em :: {}
  , codespan :: {}
  , br :: {}
  , del :: {}
  )

type LexerImpl = String -> EitherV String (Array TokenImpl)

foreign import lexerImpl :: LexerImpl

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
    [ TSB.tsTypeAlias Tok "LexerImpl" (Proxy :: _ LexerImpl)
    ]