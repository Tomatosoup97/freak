module Eval where

import qualified Data.Map as Map
import AST
import TargetAST
import CommonEval
import Types

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
            DLambda fun -> fun [argDValue]
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
        where funcRecord [xVal] = let env' = extendEnv env x xVal in eval env' c
    UVal UUnit -> return DUnit
    UVal (UPair e1 e2) -> do
        v1 <- eval env (UVal e1)
        v2 <- eval env (UVal e2)
        return $ DPair v1 v2
    UVal (ULabel l) -> return $ DLabel l


runEval :: UComp -> Either Error DValue
runEval = eval Map.empty
