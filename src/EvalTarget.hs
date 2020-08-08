{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Eval where

import qualified Data.Map as Map
import Control.Monad.Except
import AST
import TargetAST
import CommonEval
import Types

eval :: Env -> UComp -> ExceptT Error IO DValue
eval env c = case c of
    USplit l x y pair comp ->
         eval env (UVal pair) >>= \(DPair lv rv) ->
        let extEnv = Map.insert y rv (Map.insert x lv env) in
        eval extEnv comp
    UApp fnComp argValue -> do
        fnVal <- eval env fnComp
        argDValue <- eval env argValue
        case fnVal of
            DLambda fun -> fun [argDValue]
            e -> throwError $ EvalError $ "Application of non-lambda term " ++ show e
    UIf e tC fC ->
        eval env (UVal e) >>= \(DNum n) ->
        eval env (if n > 0 then tC else fC)
    UCase label caseLabel tComp y fComp -> do
        l <- eval env (UVal label)
        if l == DLabel caseLabel
        then eval env tComp
        else eval (Map.insert y l env) fComp
    UAbsurd v -> throwError $ absurdErr v
    UTopLevelEffect l -> throwError $ absurdErr (ULabel l)
    ULet x varComp comp -> do
        varVal <- eval env varComp
        let env' = extendEnv env x varVal
        eval env' comp
    -- Values
    UVal (UBinOp op e1 e2) -> do
        eval env (UVal e1) >>= \(DNum n1) ->
            eval env (UVal e2) >>= \(DNum n2) ->
            return $ DNum $ convOp op (n1, n2)
    UVal (UVar x) -> case Map.lookup x env of
        Just v -> return v
        Nothing -> throwError $ unboundVarErr x
    UVal (UNum n) -> return (DNum n)
    UVal (UStr s) -> return (DStr s)
    UVal (ULambda x c) -> return $ DLambda funcRecord
        where funcRecord [xVal] = let env' = extendEnv env x xVal in eval env' c
    UVal UUnit -> return DUnit
    UVal (UPair e1 e2) -> do
        v1 <- eval env (UVal e1)
        v2 <- eval env (UVal e2)
        return $ DPair v1 v2
    UVal (ULabel l) -> return $ DLabel l

type EvalMonad = IO (Either Error DValue)

-- instance Show EvalMonad where
--     show m = "x"

-- instance Eq EvalMonad where
--     m1 == m2 = True

runEval :: UComp -> EvalMonad
runEval c = runExceptT (eval Map.empty c)
