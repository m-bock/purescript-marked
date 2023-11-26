module Marked.Bindings
  ( Align
  , Blockquote
  , Code
  , Heading
  , Hr
  , Link
  , Links
  , List
  , ListItem
  , Paragraph
  , Space
  , StringLit(..)
  , Table
  , TableCell
  , Text
  , Token(..)
  , lexer
  , tsModules
  ) where

import Prelude

import DTS as DTS
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant.Encodings.Flat (VariantEncodedFlat)
import Foreign.Object (Object)
import LabeledData.VariantLike.Class (EitherV)
import Literals as Literals
import Literals.Null (Null)
import Prim.Boolean (True)
import TsBridge (type (|&|), Mod, TsRecord)
import TsBridge as TSB
import TsBridge.Class (class TsBridge, Tok(..))
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Untagged.TypeCheck (class HasRuntimeType)
import Untagged.Union (type (|+|), UndefinedOr)

newtype StringLit a = StringLit (Literals.StringLit a)

instance IsSymbol sym => TsBridge (StringLit sym) where
  tsBridge _ = TSB.tsBridge (Proxy :: _ (Literals.StringLit sym))

instance IsSymbol sym => HasRuntimeType (StringLit sym) where
  hasRuntimeType _ val =
    (unsafeCoerce val :: String) == reflectSymbol (Proxy :: _ sym)

-------------------------------------------------------------------------------
--- FFI
-------------------------------------------------------------------------------

newtype Token = Token
  ( VariantEncodedFlat "type"
      ( space :: Space
      , code :: Code
      , heading :: Heading
      , table :: Table
      , hr :: Hr
      , blockquote :: Blockquote
      , list :: List
      , list_item :: ListItem
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

type Space = { raw :: String }

type Code = TsRecord
  ( raw :: Mod () String
  , codeBlockStyle :: Mod (optional :: True) (UndefinedOr (StringLit "indented"))
  , lang :: Mod (optional :: True) (UndefinedOr String)
  , text :: Mod () String
  )

type Heading =
  { raw :: String
  , depth :: Number
  , text :: String
  , tokens :: Array Token
  }

type Table =
  { raw :: String
  , align :: Array Align
  , header :: Array TableCell
  , rows :: Array (Array TableCell)
  }

type Align = StringLit "center" |+| StringLit "left" |+| StringLit "right" |+| Null

type TableCell =
  { text :: String
  , tokens :: Array Token
  }

type Hr =
  { raw :: String
  }

---

type Blockquote =
  { raw :: String
  , text :: String
  , tokens :: Array Token
  }

type List =
  { raw :: String
  , ordered :: Boolean
  , start :: Number |+| (StringLit "")
  , loose :: Boolean
  , items :: Array ListItem
  }

type ListItem = TsRecord
  ( raw :: Mod () String
  , task :: Mod () Boolean
  , checked :: Mod (optional :: True) (UndefinedOr Boolean)
  , loose :: Mod () Boolean
  , text :: Mod () String
  , tokens :: Mod () (Array Token)
  )

type Paragraph = TsRecord
  ( raw :: Mod () String
  , pre :: Mod (optional :: True) (UndefinedOr Boolean)
  , text :: Mod () String
  , tokens :: Mod () (Array Token)
  )

type Text = { text :: String }

type Links =
  { links :: Object Link
  }

type Link =
  { href :: Nullable String
  , title :: Nullable String
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