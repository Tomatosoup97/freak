{-# LANGUAGE ExistentialQuantification #-}
module Typechecker where

import AST
import Types
import Parser -- TODO

import Control.Monad
import qualified Data.Map as Map


type TypeEnv = Map.Map Var Type

data Error = TypeError String
    deriving (Show)


typeError :: forall a. Show a => Expr -> a -> Error
typeError e t = TypeError (typeErrorMsg $ show e ++ ". Expected " ++ show t)
    where typeErrorMsg str = "Unsupported operand type for " ++ str


extendTypeEnv :: TypeEnv -> Var -> Type -> TypeEnv
extendTypeEnv env var value = Map.insert var value env


compareTypes :: forall a. (Eq a, Show a) => Expr -> a -> a -> Either Error a
compareTypes e t1 t2
    | t1 == t2 = Right t2
    | otherwise = Left $ typeError e t1


infer_value :: TypeEnv -> Value -> Either Error ValueType
infer_value env (VNum _) = return TInt
infer_value env (VBool _) = return TBool
infer_value env (VVar x) = case Map.lookup x env of
    Just val -> case val of
        TVal tv -> return tv
        _ -> Left $ TypeError ("Variable should be of typed to value: " ++ x)
    Nothing -> Left $ TypeError ("Unbound variable " ++ x)
infer_value env (VLambda var varT comp) =
    let extEnv = extendTypeEnv env var (TVal varT) in
    infer_comp extEnv comp >>= return . (TLambda varT)


infer_expr :: TypeEnv -> Expr -> Either Error ValueType
infer_expr env (EVal v) = infer_value env v
infer_expr env (EBinOp op e1 e2) = do
    t1 <- infer_expr env e1
    t2 <- infer_expr env e2
    compareTypes e1 TInt t1
    compareTypes e2 TInt t2
    return t1


infer_comp :: TypeEnv -> Comp -> Either Error CompType
infer_comp env (CLet var c1 c2) = do
    (TComp varT effT) <- infer_comp env c1
    let extEnv = extendTypeEnv env var (TVal varT)
    infer_comp extEnv c2
infer_comp env (CRet e) = do
    te <- infer_expr env e
    return $ TComp te RowType
infer_comp env (CApp v1 v2) = do
    appT <- infer_value env v1
    case appT of
        (TLambda varT ct) -> do
            argT <- infer_value env v2
            compareTypes (EVal v2) varT argT
            return ct
        _ -> Left $ TypeError ("Application of non-lambda term")


infer_type :: TypeEnv -> Term -> Either Error Type
infer_type env (TermComp c) = liftM TC (infer_comp env c)
infer_type env (TermExpr e) = liftM TVal (infer_expr env e)


typecheck :: Term -> Either Error Type
typecheck term = infer_type Map.empty term
