{-# LANGUAGE ExistentialQuantification #-}
module Typechecker where

import AST
import Types
import Parser -- TODO

import Control.Monad
import qualified Data.Map as Map


type TypeEnv = Map.Map Var Type

newtype Error = TypeError String
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


inferValue :: TypeEnv -> Value -> Either Error ValueType
inferValue env (VNum _) = return TInt
inferValue env (VBool _) = return TBool
inferValue env (VVar x) = case Map.lookup x env of
    Just val -> case val of
        TVal tv -> return tv
        _ -> Left $ TypeError ("Variable should be of typed to value: " ++ x)
    Nothing -> Left $ TypeError ("Unbound variable " ++ x)
inferValue env (VLambda var varT comp) =
    let extEnv = extendTypeEnv env var (TVal varT) in
    TLambda varT <$> inferComp extEnv comp


inferExpr :: TypeEnv -> Expr -> Either Error ValueType
inferExpr env (EVal v) = inferValue env v
inferExpr env (EBinOp op e1 e2) = do
    t1 <- inferExpr env e1
    t2 <- inferExpr env e2
    compareTypes e1 TInt t1
    compareTypes e2 TInt t2
    return t1


inferComp :: TypeEnv -> Comp -> Either Error CompType
inferComp env (CLet var c1 c2) = do
    (TComp varT effT) <- inferComp env c1
    let extEnv = extendTypeEnv env var (TVal varT)
    inferComp extEnv c2
inferComp env (EReturn e) = do
    te <- inferExpr env e
    return $ TComp te RowType
inferComp env (CApp v1 v2) = do
    appT <- inferValue env v1
    case appT of
        (TLambda varT ct) -> do
            argT <- inferValue env v2
            compareTypes (EVal v2) varT argT
            return ct
        _ -> Left $ TypeError "Application of non-lambda term"


inferType :: TypeEnv -> Term -> Either Error Type
inferType env (TermComp c) = fmap TC (inferComp env c)
inferType env (TermExpr e) = fmap TVal (inferExpr env e)


typecheck :: Term -> Either Error Type
typecheck = inferType Map.empty
