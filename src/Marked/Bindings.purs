module Marked.Bindings where

import DTS as DTS
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Variant.Encodings.Flat (VariantEncodedFlat)
import Foreign.Object (Object)
import LabeledData.VariantLike.Class (EitherV)
import Literals (StringLit)
import Literals.Null (Null)
import Prim.Boolean (True)
import TsBridge (type (|&|), Mod, TsRecord)
import TsBridge as TSB
import TsBridge.Class (class TsBridge, Tok(..))
import Untagged.Union (type (|+|), UndefinedOr)

-------------------------------------------------------------------------------
--- FFI
-------------------------------------------------------------------------------

newtype Token = Token
  ( VariantEncodedFlat "type"
      ( space :: {}
      , code :: Code
      , heading :: Heading
      , table :: Table
      , hr :: {}
      , blockquote :: Blockquote
      , list :: List
      , list_item :: {}
      , paragraph :: Paragraph
      , html :: {}
      , text :: Text
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
  )

type Code = TsRecord
  ( lang :: Mod (optional :: True) (UndefinedOr String)
  , text :: Mod () String
  )

type Heading = { depth :: Number, tokens :: Array Token }

type Table =
  { align ::
      Array
        (StringLit "center" |+| StringLit "left" |+| StringLit "right" |+| Null)
  }

type Blockquote = { text :: String, tokens :: Array Token }

type List = { ordered :: Boolean }

type Paragraph = { tokens :: Array Token, text :: String }

type Text = { text :: String }

type Links =
  { links ::
      Object
        { href :: Nullable String
        , title :: Nullable String
        }
  }

foreign import lexer :: String -> EitherV String (Array Token |&| Links)

-------------------------------------------------------------------------------
--- TsBridge
-------------------------------------------------------------------------------

derive instance Newtype Token _

instance TsBridge Token where
  tsBridge x = TSB.tsBridgeNewtype
    Tok
    { moduleName
    , typeName: "Token"
    , typeArgs: []
    }
    x

moduleName :: String
moduleName = "Marked.Bindings"

tsModules :: Either TSB.AppError (Array DTS.TsModuleFile)
tsModules =
  TSB.tsModuleFile moduleName
    [ TSB.tsTypeAliasesFromValues Tok { lexer }
    ]