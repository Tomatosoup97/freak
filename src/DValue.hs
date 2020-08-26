module DValue where

import qualified Data.Map as Map
import Control.Monad.Except
import AST
import TargetAST
import Types

type Env = Map.Map Var DValue

type FuncRecord = Env -> [DValue] -> [Cont] -> EvalMonad DValue

data DValue
    = DNum Integer
    | DStr String
    | DLambda FuncRecord
    | DUnit
    | DPair DValue DValue
    | DLabel Label

instance Show DValue where
    show (DNum n) = show n
    show (DStr s) = show s
    show (DLambda _) = "lambda"
    show  DUnit = "()"
    show (DPair l r) = inParens $ show l ++ ", " ++ show r
    show (DLabel l) = inParens $ "L: " ++ show l

instance Eq DValue where
    DNum n == DNum n' = n == n'
    DStr s == DStr s' = s == s'
    DUnit == DUnit = True
    DPair a b == DPair a' b' = a == a' && b == b'
    DLabel l == DLabel l' = l == l'
    _ == _ = False
