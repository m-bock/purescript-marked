module Marked.Bindings
  ( Align
  , Blockquote
  , Br
  , Code
  , Codespan
  , Def
  , Del
  , Em
  , Escape
  , Heading
  , Hr
  , Html
  , Image
  , Link
  , LinkRef
  , Links
  , List
  , ListItem
  , Paragraph
  , Space
  , StringLit(..)
  , Strong
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
      , html :: Html
      , text :: Text
      -- , def :: Def
      -- , escape :: Escape
      -- , text :: Text
      -- , link :: Link
      -- , image :: Image
      -- , strong :: Strong
      -- , em :: Em
      -- , codespan :: Codespan
      -- , br :: Br
      -- , del :: Del
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

type Html = TsRecord
  ( raw :: Mod () String
  , pre :: Mod () Boolean
  , block :: Mod () Boolean
  , text :: Mod () String
  , inLink :: Mod (optional :: True) Boolean
  , inRawBlock :: Mod (optional :: True) Boolean
  )

type Text = TsRecord
  ( raw :: Mod () String
  , text :: Mod () String
  , tokens :: Mod (optional :: True) (UndefinedOr (Array Token))
  , inLink :: Mod (optional :: True) Boolean
  , inRawBlock :: Mod (optional :: True) Boolean
  )

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
  , title :: String
  , text :: String
  , tokens :: Array Token
  }

type Image =
  { raw :: String
  , href :: String
  , title :: String
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

type Links =
  { links :: Object LinkRef
  }

type LinkRef =
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