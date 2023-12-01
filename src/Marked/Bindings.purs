-- | Low level bindings to marked.js
-- | Describes the JS API as it is in terms of PureScript types.
-- | There is no performance overhead compared to the JS API.
-- | Statically checked against TS types by ts-bridge.
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
  , Strong
  , Table
  , TableCell
  , Tag
  , Text
  , Token(..)
  , lexer
  , tsModules
  ) where

import DTS as DTS
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Variant.Encodings.Flat (VariantEncodedFlat)
import Foreign.Object (Object)
import LabeledData.VariantLike.Class (EitherV)
import Literals.Null (Null)
import Prim.Boolean (False, True)
import TsBridge (type (|&|), type (~), TsRecord, Opt)
import TsBridge as TSB
import TsBridge.Class (class TsBridge, Tok(..))
import TsBridge.Types.Lit (Lit)
import TsBridge.Types.RecordUnion (RecordUnion)
import Untagged.Union (type (|+|), UndefinedOr)

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
      , html :: RecordUnion (Tag |+| Html)
      , text :: RecordUnion (Tag |+| Text)
      , def :: Def
      , escape :: Escape
      , link :: Link
      , image :: Image
      , strong :: Strong
      , em :: Em
      , codespan :: Codespan
      , br :: Br
      , del :: Del
      )
  )

type Space = { raw :: String }

type Code = TsRecord
  ( raw :: () ~ String
  , codeBlockStyle :: Opt ~ UndefinedOr (Lit "indented" String)
  , lang :: Opt ~ UndefinedOr String
  , text :: () ~ String
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

type Align =
  Lit "center" String
    |+| Lit "left" String
    |+| Lit "right" String
    |+| Null

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
  , start :: Number |+| (Lit "" String)
  , loose :: Boolean
  , items :: Array ListItem
  }

type ListItem = TsRecord
  ( raw :: () ~ String
  , task :: () ~ Boolean
  , checked :: (Opt) ~ UndefinedOr Boolean
  , loose :: () ~ Boolean
  , text :: () ~ String
  , tokens :: () ~ (Array Token)
  )

type Paragraph = TsRecord
  ( raw :: () ~ String
  , pre :: (Opt) ~ UndefinedOr Boolean
  , text :: () ~ String
  , tokens :: () ~ Array Token
  )

type Html =
  { raw :: String
  , pre :: Boolean
  , block :: Lit True Boolean
  , text :: String
  }

type Text = TsRecord
  ( raw :: () ~ String
  , text :: () ~ String
  , tokens :: (Opt) ~ UndefinedOr (Array Token)
  )

type Tag =
  { raw :: String
  , inLink :: Boolean
  , inRawBlock :: Boolean
  , block :: Lit False Boolean
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
  , title :: Nullable String -- in TS wrongly defined as String
  , text :: String
  , tokens :: Array Token
  }

type Image =
  { raw :: String
  , href :: String
  , title :: Nullable String -- in TS wrongly defined as String
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