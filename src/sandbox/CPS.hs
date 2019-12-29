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
    | CRecordRow (RecordRow CValue)
    | CVariantRow (VariantRow CValue)
    deriving (Show)

data ContComp
    = CPSApp CValue [CValue]
    | CPSFix Var [Var] ContComp ContComp
    | CPSBinOp BinaryOp CValue CValue Var ContComp
    | CPSValue CValue
    | CPSLet Var CValue ContComp
    | CPSSplit Label Var Var CValue ContComp
    | CPSCase CValue Label Var ContComp Var ContComp
    deriving (Show)

type CPSMonad a = ExceptT Error (State Int) a

type Cont = CValue -> CPSMonad ContComp

initialCont :: Cont
initialCont = return . CPSValue

freshVar :: CPSMonad Var
freshVar = do
    n <- get
    put (n+1)
    return $ "__meta" ++ (show n) -- todo: this should be unique!


cpsRecordRow :: RecordRow Value -> [(Label, CValue)] -> Cont -> CPSMonad ContComp
-- TODO: use Functor instance
cpsRecordRow (RecordRowUnit) cvs c = c $ CRecordRow contCompRow
    where contCompRow = foldl consRow RecordRowUnit cvs
          consRow row (l, cv) = RecordRowExtend l cv row
cpsRecordRow (RecordRowExtend l v r) cvs c = cps (EVal v) cont
    where cont cval = cpsRecordRow r ((l, cval):cvs) c


cps :: Comp -> Cont -> CPSMonad ContComp
cps e c = case e of
    EVal (VVar x) -> c $ CVar x
    EVal (VNum n) -> c $ CNum n
    EVal (VRecordRow row) -> cpsRecordRow row [] c
    EVal (VVariantRow (VariantRow t l v)) ->
        cps (EVal v) (\cv -> c $ CVariantRow (VariantRow t l cv))
    EVal (VLambda x _ body) -> do
        fnvar <- freshVar
        contVar <- freshVar
        convBody <- cps body (\v -> return $ CPSApp (CVar contVar) [v])
        contComp <- c $ CVar fnvar
        return $ CPSFix fnvar [x, contVar] convBody contComp
    EVal (VFix g x body) -> do
        contVar <- freshVar
        convBody <- cps body (\v -> return $ CPSApp (CVar contVar) [v])
        contComp <- c $ CVar g
        return $ CPSFix g [x, contVar] convBody contComp
    EBinOp op e1 e2 -> do
        opVar <- freshVar
        contComp <- c $ CVar opVar
        cps e1 (\v1 -> cps e2 (\v2 -> return $ CPSBinOp op v1 v2 opVar contComp))
    EApp e1 e2 -> do
        resVar <- freshVar
        resArg <- freshVar
        resBody <- c $ CVar resArg
        contComp <- cps e1 (\f -> cps e2 (\v -> return $ CPSApp f [v, CVar resVar]))
        return $ CPSFix resVar [resArg] resBody contComp
    ELet x varComp e -> do
        convE <- cps e c -- is passing continuation c here correct?
        cps varComp (\v -> return $ CPSLet x v convE)
    ESplit l x y row comp -> do
        cont <- cps comp c
        cps (EVal row) (\convRow -> return $ CPSSplit l x y convRow cont)
    ECase variant l x tComp y fComp -> do
        tCont <- cps tComp c
        fCont <- cps fComp c
        cps (EVal variant) (
            \convVariant -> return $ CPSCase convVariant l x tCont y fCont)
    EReturn v -> cps (EVal v) c
    EAbsurd v -> throwError $ CPSError "Absurd; divergent term"


runCPS :: Comp -> Either Error ContComp
runCPS e = evalState (runExceptT $ cps e initialCont) 0
