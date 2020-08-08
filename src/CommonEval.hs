module CommonEval where

import qualified Data.Map as Map
import AST
import TargetAST
import Types

type FuncRecord = [DValue] -> Either Error DValue

type Env = Map.Map Var DValue

extendEnv :: Env -> Var -> DValue -> Env
extendEnv env x v = Map.insert x v env

unboundVarErr :: String -> Either Error DValue
unboundVarErr x = Left $ EvalError $ "Unbound variable " ++ x

absurdErr :: Either Error DValue
absurdErr = Left $ EvalError "Absurd; divergent term"

boolToInt :: Bool -> Integer
boolToInt b = if b then 1 else 0

convOp :: BinaryOp -> (Integer, Integer) -> Integer
convOp BAdd = uncurry (+)
convOp BMul = uncurry (*)
convOp BDiv = uncurry div
convOp BSub = uncurry (-)
convOp BLte = boolToInt . uncurry (<=)
convOp BLt = boolToInt . uncurry (<)
convOp BGte = boolToInt . uncurry (>=)
convOp BGt = boolToInt . uncurry (>)
convOp BEq = boolToInt . uncurry (==)
convOp BNe = boolToInt . uncurry (/=)

data DValue
    = DNum Integer
    | DStr String
    | DLambda FuncRecord
    | DUnit
    | DPair DValue DValue
    | DLabel Label

instance Show DValue where
    show (DNum n) = show n
    show (DStr s) = "\"" ++ show s ++ "\""
    show (DLambda _) = "lambda"
    show  DUnit = "()"
    show (DPair l r) = "(" ++ show l ++ ", " ++ show r ++ ")"
    show (DLabel l) = "L: " ++ show l

instance Eq DValue where
    DNum n == DNum n' = n == n'
    DStr s == DStr s' = s == s'
    DUnit == DUnit = True
    DPair a b == DPair a' b' = a == a' && b == b'
    DLabel l == DLabel l' = l == l'
    _ == _ = False
