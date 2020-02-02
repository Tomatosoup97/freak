module Eval where

import qualified Data.Map as Map
import AST
import TargetAST
import Types


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
    | DLambda FuncRecord
    | DUnit
    | DPair DValue DValue
    | DLabel Label

instance Show DValue where
    show (DNum n) = show n
    show (DLambda _) = "lambda"
    show  DUnit = "()"
    show (DPair l r) = "(" ++ show l ++ ", " ++ show r ++ ")"
    show (DLabel l) = "L: " ++ show l

instance Eq DValue where
    DNum n == DNum n' = n == n'
    DUnit == DUnit = True
    DPair a b == DPair a' b' = a == a' && b == b'
    DLabel l == DLabel l' = l == l'
    _ == _ = False

type FuncRecord = DValue -> Either Error DValue

type Env = Map.Map Var DValue

extendEnv :: Env -> Var -> DValue -> Env
extendEnv env x v = Map.insert x v env

eval :: Env -> UComp -> Either Error DValue
eval env c = case c of
    USplit l x y pair comp -> do
        (DPair lv rv) <- eval env (UVal pair)
        let extEnv = Map.insert y rv (Map.insert x lv env)
        eval extEnv comp
    UApp fnComp argValue -> do
        fnVal <- eval env fnComp
        argDValue <- eval env argValue
        case fnVal of
            DLambda fun -> fun argDValue
            -- TODO: handle URec
            -- DRec g x comp -> Left $ EvalError "Recursor not supported yet!"
            --     let extEnv = Map.insert x argVal (Map.insert g fnVal env) in
            --     eval extEnv comp
            e -> Left $ EvalError $ "Application of non-lambda term " ++ show e
    UIf e tC fC -> do
        (DNum n) <- eval env (UVal e)
        eval env (if n > 0 then tC else fC)
    UCase label caseLabel tComp y fComp -> do
        l <- eval env (UVal label)
        if l == DLabel caseLabel
        then eval env tComp
        else eval (Map.insert y l env) fComp
    UAbsurd _ -> absurdErr
    ULet x varComp comp -> do
        varVal <- eval env varComp
        let env' = extendEnv env x varVal
        eval env' comp
    -- Values
    UVal (UBinOp op e1 e2) -> do
        (DNum n1) <- eval env (UVal e1)
        (DNum n2) <- eval env (UVal e2)
        return $ DNum $ convOp op (n1, n2)
    UVal (UVar x) -> case Map.lookup x env of
        Just v -> return v
        Nothing -> unboundVarErr x
    UVal (UNum n) -> return (DNum n)
    UVal (ULambda x c) -> return $ DLambda funcRecord
        where funcRecord xVal = let env' = extendEnv env x xVal in eval env' c
    UVal UUnit -> return DUnit
    UVal (UPair e1 e2) -> do
        v1 <- eval env (UVal e1)
        v2 <- eval env (UVal e2)
        return $ DPair v1 v2
    UVal (ULabel l) -> return $ DLabel l
    -- | URec Var Var UComp


runEval :: UComp -> Either Error DValue
runEval = eval Map.empty
