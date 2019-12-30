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
    | CPair CValue CValue -- todo
    | CPSLabel Label -- todo
    deriving (Show)

data ContComp
    = CPSApp CValue [CValue]
    | CPSFix Var [Var] ContComp ContComp
    | CPSBinOp BinaryOp CValue CValue Var ContComp
    | CPSValue CValue
    | CPSLet Var CValue ContComp
    | CPSSplit Label Var Var CValue ContComp
    | CPSCase CValue Label Var ContComp Var ContComp
    | CPSAbsurd Var CValue
    deriving (Show)

type CPSMonad a = ExceptT Error (State Int) a

type EffCont = CValue -> CPSMonad ContComp
type PureCont = CValue -> EffCont -> CPSMonad ContComp

initialPureCont :: PureCont
initialPureCont v h = (return . CPSValue) v -- todo: point-free?

initialEffCont :: EffCont
initialEffCont (CRecordRow (RecordRowExtend "1" z r)) =
    return $ CPSAbsurd "x" z -- todo: syntactic sugar "1" -- todo "x"?

initialState :: Int
initialState = 0

freshVar :: CPSMonad Var
freshVar = do
    n <- get
    put (n+1)
    return $ "__meta" ++ (show n) -- todo: this should be unique!


-- todo: this function should be simplified
-- TODO: use Functor instance
cpsRecordRow :: RecordRow Value -> [(Label, CValue)] -> PureCont -> EffCont -> CPSMonad ContComp
cpsRecordRow (RecordRowUnit) cvs k = k $ CRecordRow contCompRow
    where contCompRow = foldl consRow RecordRowUnit cvs
          consRow row (l, cv) = RecordRowExtend l cv row
cpsRecordRow (RecordRowExtend l v r) cvs k = cps (EVal v) cont
    where cont cval = cpsRecordRow r ((l, cval):cvs) k


cps :: Comp -> PureCont -> EffCont -> CPSMonad ContComp
cps e k h = case e of
    EVal (VVar x) -> k (CVar x) h
    EVal (VNum n) -> k (CNum n) h
    EVal (VRecordRow row) -> cpsRecordRow row [] k h
    EVal (VVariantRow (VariantRow t l v)) ->
        cps (EVal v) (\cv -> \h -> k (CVariantRow (VariantRow t l cv)) h) h
    EVal (VLambda x _ body) -> do
        fnvar <- freshVar
        contVar <- freshVar
        convBody <- cps body (\v -> \h -> return $ CPSApp (CVar contVar) [v]) h
        contComp <- k (CVar fnvar) h
        return $ CPSFix fnvar [x, contVar] convBody contComp
    EVal (VFix g x body) -> do
        contVar <- freshVar
        convBody <- cps body (\v -> \h -> return $ CPSApp (CVar contVar) [v]) h
        contComp <- k (CVar g) h
        return $ CPSFix g [x, contVar] convBody contComp
    EBinOp op e1 e2 -> do
        opVar <- freshVar
        contComp <- k (CVar opVar) h
        cps e1 (\v1 -> \h -> cps e2 (\v2 -> \h -> return $ CPSBinOp op v1 v2 opVar contComp) h) h
    EApp e1 e2 -> do
        resVar <- freshVar
        resArg <- freshVar
        resBody <- k (CVar resArg) h
        contComp <- cps e1 (\f -> \h -> cps e2 (\v -> \h -> return $ CPSApp f [v, CVar resVar]) h) h
        return $ CPSFix resVar [resArg] resBody contComp
    ELet x varComp e -> do
        convE <- cps e k h -- is passing continuation here correct?
        cps varComp (\v -> \h -> return $ CPSLet x v convE) h
    ESplit l x y row comp -> do
        cont <- cps comp k h
        cps (EVal row) (\convRow -> \h -> return $ CPSSplit l x y convRow cont) h
    ECase variant l x tComp y fComp -> do
        tCont <- cps tComp k h
        fCont <- cps fComp k h
        cps (EVal variant) (
            \convVariant -> \h -> return $ CPSCase convVariant l x tCont y fCont) h
    EReturn v -> cps (EVal v) k h
    EAbsurd v -> do -- todo: not sure if that's desired translation
        var <- freshVar
        cont <- k (CVar var) h
        cps (EVal v) (\cv -> \h -> return $ CPSAbsurd var cv) h
    -- Algebraic effects
    EDo l v -> do
        fnvar <- freshVar
        x <- freshVar
        lambdaComp <- k (CVar x) h
        let exponential cv = CPair cv (CVar fnvar)
        contComp <- cps (EVal v) (\cv -> \_ ->
            h $ CPair (CPSLabel l) (exponential cv)) h
        return $ CPSFix fnvar [x] lambdaComp contComp
    EHandle body handler ->
        cps body (cpsHRet k h (hret handler)) (cpsHOps k h (hops handler))


cpsHRet :: PureCont -> EffCont -> Handler -> PureCont
cpsHRet k h (HRet x comp) = \x -> \h' -> do
    xVar <- freshVar
    convComp <- cps comp k h
    return $ CPSLet xVar x convComp


cpsHOps :: PureCont -> EffCont -> [AlgebraicOp] -> EffCont
cpsHOps k h ops = \(CPair l (CPair p r)) -> undefined


runCPS :: Comp -> Either Error ContComp
runCPS e = evalState (runExceptT cpsTerm) initialState
    where cpsTerm = cps e initialPureCont initialEffCont
