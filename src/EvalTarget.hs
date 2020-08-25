{-# LANGUAGE FlexibleContexts, FlexibleInstances, LambdaCase #-}
module Eval where

import qualified Data.Map as Map
import Control.Monad.Except
import AST
import TargetAST
import DValue
import Types

type FuncRecord = [DValue] -> ExceptT Error IO DValue

type Env = Map.Map Var DValue

extendEnv :: Env -> Var -> DValue -> Env
extendEnv env x v = Map.insert x v env

unboundVarErr :: String -> Error
unboundVarErr x = EvalError $ "Unbound variable " ++ x

absurdErr :: Show a => a -> Error
absurdErr x = EvalError $ "Absurd; divergent term: " ++ show x

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


handleEffect :: EffLabel -> DValue -> EvalMonad DValue
handleEffect (EffL l) v
    -- TODO: Write down in latex this functionality
    | l == "Print" = do
        (lift . print) v
        return DUnit
    | l == "ReadLine" = do
        input <- lift getLine
        return $ DStr input
    | l == "ReadFile" = case v of
        (DStr filename) -> do
            contents <- lift $ readFile filename
            return $ DStr contents
        _ -> throwError $ EvalError $ l ++ " effect accepts a single filename string"
    | l == "WriteFile" = case v of
        (DPair (DStr filename) (DStr contents)) -> do
            lift $ writeFile filename contents
            return DUnit
        _ -> throwError $ EvalError $ l ++ " effect accepts a pair of (filename, contents) strings"
    | l == "AppendFile" = case v of
        (DPair (DStr filename) (DStr contents)) -> do
            lift $ appendFile filename contents
            return DUnit
        _ -> throwError $ EvalError $ l ++ " effect accepts a pair of (filename, contents) strings"
    | otherwise = throwError $ absurdErr (DPair (DLabel l) v)


eval :: Env -> UComp -> EvalMonad DValue
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
            e -> throwError $ EvalError $ "Application of non-lambda term " ++ show e ++ " in " ++ show c
    UIf e tC fC ->
        eval env (UVal e) >>= \(DNum n) ->
        eval env (if n > 0 then tC else fC)
    UCase label caseLabel tComp y fComp -> do
        l <- eval env (UVal label)
        if l == DLabel caseLabel
        then eval env tComp
        else eval (Map.insert y l env) fComp
    ULet x varComp comp -> do
        varVal <- eval env varComp
        let env' = extendEnv env x varVal
        eval env' comp
    UTopLevelEffect l v ->
        eval env (UVal v) >>= handleEffect l
    UAbsurd v -> throwError $ absurdErr v
    -- Values
    UVal (UBinOp op e1 e2) -> do
        eval env (UVal e1) >>= \(DNum n1) ->
            eval env (UVal e2) >>= \(DNum n2) ->
            return $ DNum $ convOp op (n1, n2)
    UVal (UVar x) -> case Map.lookup x env of
        Just v -> return v
        Nothing -> throwError $ unboundVarErr x
    UVal (UPair e1 e2) -> do
        v1 <- eval env (UVal e1)
        v2 <- eval env (UVal e2)
        return $ DPair v1 v2
    UVal (UFst e) -> eval env (UVal e) >>= \case
        DPair v1 _ -> return v1
        v -> throwError $ EvalError $ "First projection on expression that is not a pair: " ++ show v
    UVal (USnd e) -> eval env (UVal e) >>= \case
        DPair _ v2 -> return v2
        v -> throwError $ EvalError $ "Second projection on expression that is not a pair: " ++ show v
    UVal (ULambda x c) -> return $ DLambda funcRecord
        where funcRecord [xVal] = let env' = extendEnv env x xVal in eval env' c
    UVal (ULabel l) -> return $ DLabel l
    UVal (UEffLabel (EffL l)) -> return $ DLabel l
    UVal (UEffLabel (CoeffL l)) -> return $ DLabel l
    UVal (UNum n) -> return (DNum n)
    UVal (UStr s) -> return (DStr s)
    UVal UUnit -> return DUnit


runEval :: UComp -> EvalResMonad DValue
runEval c = runExceptT (eval Map.empty c)
