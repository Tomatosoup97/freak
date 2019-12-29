module AST where

import qualified Data.Map as Map
import Types

type Var = String
type Label = String

data VariantRow a = VariantRow RowType Label a
    deriving (Show, Eq)

data RecordRow a
    = RecordRowUnit
    | RecordRowExtend Label a (RecordRow a)
    deriving (Show, Eq)

instance Functor RecordRow where
    fmap _ RecordRowUnit = RecordRowUnit
    fmap f (RecordRowExtend l a r) = RecordRowExtend l (f a) (fmap f r)

data Value
    = VVar Var
    | VNum Integer
    | VBool Bool
    | VLambda Var ValueType Comp
    | VFix Var Var Comp
    | VRecordRow (RecordRow Value)
    | VVariantRow (VariantRow Value)
    deriving (Show, Eq)

data BinaryOp
    = BAdd
    | BMul
    deriving (Show, Eq)

data Comp
    = EBinOp BinaryOp Comp Comp
    | EVal Value
    | ELet Var Comp Comp
    | EApp Comp Comp
    | ESplit Label Var Var Value Comp
    | ECase Value Label Var Comp Var Comp
    | EReturn Value
    | EAbsurd Value
    deriving (Show, Eq)
