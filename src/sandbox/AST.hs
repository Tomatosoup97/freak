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
    | VLambda Var ValueType Expr
    | VFix Var Var Expr
    | VRecordRow (RecordRow Value)
    | VVariantRow (VariantRow Value)
    deriving (Show, Eq)

data BinaryOp
    = BAdd
    | BMul
    deriving (Show, Eq)

data Expr
    = EBinOp BinaryOp Expr Expr
    | EVal Value
    | ELet Var Expr Expr
    | EApp Expr Expr
    | ESplit Label Var Var Value Expr
    deriving (Show, Eq)
