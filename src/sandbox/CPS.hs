module CPS where

import Control.Monad.Except
import Control.Monad.State
import AST
import Types


data Error
    = EvalError String
    | CPSError String
    deriving (Show)

data CValue
    = CVar Var
    | CNum Integer
    deriving (Show)

data CExp
    = CPSApp CValue [CValue]
    | CPSFix Var [Var] CExp CExp
    | CPSBinOp BinaryOp CValue CValue Var CExp
    | CPSValue CValue
    -- | CPSLet Var CExp CExp
    deriving (Show)

type CPSMonad a = ExceptT Error (State Int) a

type Cont = CValue -> CPSMonad CExp

initialCont :: Cont
initialCont = return . CPSValue

freshVar :: CPSMonad Var
freshVar = do
    n <- get
    put (n+1)
    return $ "__x" ++ (show n) -- todo: this should be unique!

cps :: Expr -> Cont -> CPSMonad CExp
cps e c = case e of
    EVal (VVar x) -> c $ CVar x
    EVal (VNum n) -> c $ CNum n
    EVal (VLambda x _ body) -> do
        fnvar <- freshVar
        contVar <- freshVar
        convBody <- cps body (\v -> return $ CPSApp (CVar contVar) [v])
        contExpr <- c $ CVar fnvar
        return $ CPSFix fnvar [x, contVar] convBody contExpr
    EVal (VFix g x body) -> do
        contVar <- freshVar
        convBody <- cps body (\v -> return $ CPSApp (CVar contVar) [v])
        contExpr <- c $ CVar g
        return $ CPSFix g [x, contVar] convBody contExpr
    EBinOp op e1 e2 -> do
        opVar <- freshVar
        contExpr <- c $ CVar opVar
        cps e1 (\v1 -> cps e2 (\v2 -> return $ CPSBinOp op v1 v2 opVar contExpr))
    EApp e1 e2 -> do
        resVar <- freshVar
        resArg <- freshVar
        resBody <- c $ CVar resArg
        contExpr <- cps e1 (\f -> cps e2 (\v -> return $ CPSApp f [v, CVar resVar]))
        return $ CPSFix resVar [resArg] resBody contExpr
