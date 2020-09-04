{-# LANGUAGE FlexibleContexts, FlexibleInstances, LambdaCase #-}
module Eval where

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import AST
import TargetAST
import DValue
import Types

extendEnv :: Env -> Var -> DValue -> Env
extendEnv env x v = Map.insert x v env

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
handleEffect (CoeffL l) v
    -- TODO: Write down in latex this functionality
    | l == "Print" = do
        (liftIO . print) v
        return DUnit
    | l == "ReadLine" = do
        input <- liftIO getLine
        return $ DStr input
    | l == "ReadFile" = case v of
        (DStr filename) -> do
            contents <- liftIO $ readFile filename
            return $ DStr contents
        _ -> throwError $ EvalError $ l ++ " effect accepts a single filename string"
    | l == "WriteFile" = case v of
        (DPair (DStr filename) (DStr contents)) -> do
            liftIO $ writeFile filename contents
            return DUnit
        _ -> throwError $ EvalError $ l ++ " effect accepts a pair of (filename, contents) strings"
    | l == "AppendFile" = case v of
        (DPair (DStr filename) (DStr contents)) -> do
            liftIO $ appendFile filename contents
            return DUnit
        _ -> throwError $ EvalError $ l ++ " effect accepts a pair of (filename, contents) strings"
    | otherwise = throwError $ absurdErr (DPair (DLabel l) v)
handleEffect (EffL l) v = throwError $ absurdErr (DPair (DLabel l) v)


eval :: Env -> UComp -> EvalMonad DValue
eval env c = case c of
    UApp fnComp argValue ks -> do
        fnVal <- eval env fnComp
        case fnVal of
            DLambda fun -> fun env argValue ks
            e -> throwError $ EvalError $ "Application of non-lambda term " ++ show e ++ " in " ++ show c
    UIf e tC fC ->
        eval env (UVal e) >>= \(DNum n) ->
        eval env (if n > 0 then tC else fC)
    ULet x varVal c -> do
        let varVal' = substV x varVal varVal
        let c' = subst x varVal' c
        eval env c'
    UAbsurd v -> throwError $ absurdErr v
    -- Values
    UVal (UBinOp op e1 e2) -> do
        eval env (UVal e1) >>= \case
            (DNum n1) -> eval env (UVal e2) >>= \(DNum n2) -> return $ DNum $ convOp op (n1, n2)
            (DStr s1) -> eval env (UVal e2) >>= \(DStr s2) -> case op of
                BEq -> return $ DNum $ boolToInt $ s1 == s2
                BNe -> return $ DNum $ boolToInt $ s1 /= s2
    UVal (UVar x) -> throwError $ unboundVarErr x
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
    UVal (ULambda x f) -> return $ DLambda funcRecord
        where funcRecord _ xVal = f >=> (eval env . subst x xVal)
    UVal (ULabel l) -> return $ DLabel l
    UVal (UEffLabel (EffL l)) -> return $ DLabel l
    UVal (UEffLabel (CoeffL l)) -> return $ DLabel l
    UVal (UNum n) -> return (DNum n)
    UVal (UStr s) -> return (DStr s)
    UVal UUnit -> return DUnit
    UVal (UTopLevelEffect l v) -> eval env (UVal v) >>= handleEffect l


runEval :: UComp -> EvalResMonad DValue
runEval c = evalStateT (runExceptT (eval undefined c)) 0
