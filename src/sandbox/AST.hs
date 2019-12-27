module AST where

import qualified Data.Map as Map
import Types

type Var = String
type Label = String

data Value
    = VVar Var
    | VNum Integer
    | VBool Bool
    | VLambda Var ValueType Expr
    | VFix Var Var Expr
    -- | VProductRow (Map.Map Label Value)
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
    deriving (Show, Eq)
