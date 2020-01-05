module CPS where

import Control.Monad.Except
import Control.Monad.State
import AST
import Types


data CValue
    = CVar Var
    | CNum Integer
    | CUnit
    | CPair CValue CValue
    | CLabel Label
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
initialEffCont (CPair z _) = return $ CPSAbsurd "x" z -- todo: "x" shouldn't be necessary

initialState :: Int
initialState = 0

freshVar :: CPSMonad Var
freshVar = do
    n <- get
    put (n+1)
    return $ "__meta" ++ show n -- todo: this should be unique!

consRow :: (Label, CValue) -> CValue -> CValue
consRow (l, v) rowV = CPair (CLabel l) (CPair v rowV)

jota :: Label -> CValue -> CValue
jota l = CPair (CLabel l)

cps :: Comp -> PureCont -> EffCont -> CPSMonad ContComp
cps e k h = case e of
    EVal (VVar x) -> k (CVar x) h
    EVal (VNum n) -> k (CNum n) h
    EVal (VRecordRow row) -> undefined -- todo: records are constructed using ExtendRow
    EVal VUnit -> k CUnit h
    EVal (VExtendRow l v row) -> cps (EVal v) valCont h
        where valCont cv = cps (EVal row) (rowCont cv)
              rowCont cv rowV = k ((l, cv) `consRow` rowV)
    EVal (VVariantRow (VariantRow _ l v)) -> cps (EVal v) valCont h
        where valCont cv = k (jota l cv)
    EVal (VLambda x _ body) -> do
        fnvar <- freshVar
        contVar <- freshVar
        convBody <- cps body (\v h -> return $ CPSApp (CVar contVar) [v]) h
        contComp <- k (CVar fnvar) h
        return $ CPSFix fnvar [x, contVar] convBody contComp
    EVal (VFix g x body) -> do
        contVar <- freshVar
        convBody <- cps body (\v h -> return $ CPSApp (CVar contVar) [v]) h
        contComp <- k (CVar g) h
        return $ CPSFix g [x, contVar] convBody contComp
    EVal (VBinOp op e1 e2) -> do
        opVar <- freshVar
        contComp <- k (CVar opVar) h
        let cont' v1 v2 h = return $ CPSBinOp op v1 v2 opVar contComp
        let cont v1 = cps (EVal e2) (cont' v1)
        cps (EVal e1) cont h
    EApp e1 e2 -> do
        resVar <- freshVar
        resArg <- freshVar
        resBody <- k (CVar resArg) h
        let cont' f v h = return $ CPSApp f [v, CVar resVar]
        let cont = cps (EVal e2) . cont'
        contComp <- cps (EVal e1) cont h
        return $ CPSFix resVar [resArg] resBody contComp
    ELet x varComp e -> do
        convE <- cps e k h -- is passing continuation here correct?
        cps varComp (\v h -> return $ CPSLet x v convE) h
    ESplit l x y row comp -> do
        cont <- cps comp k h
        cps (EVal row) (\convRow h -> return $ CPSSplit l x y convRow cont) h
    ECase variant l x tComp y fComp -> do
        tCont <- cps tComp k h
        fCont <- cps fComp k h
        cps (EVal variant) (
            \convVariant h -> return $ CPSCase convVariant l x tCont y fCont) h
    EReturn v -> cps (EVal v) k h
    EAbsurd v -> do -- todo: not sure if that's desired translation
        var <- freshVar
        cont <- k (CVar var) h
        cps (EVal v) (\cv h -> return $ CPSAbsurd var cv) h
    -- Algebraic effects
    EDo l v -> do
        fnvar <- freshVar
        x <- freshVar
        lambdaComp <- k (CVar x) h
        let exponential cv = CPair cv (CVar fnvar)
        contComp <- cps (EVal v) (\cv _ ->
            h $ CPair (CLabel l) (exponential cv)) h
        return $ CPSFix fnvar [x] lambdaComp contComp
    EHandle body handler ->
        cps body (cpsHRet k h (hret handler)) (cpsHOps k h handler)


cpsHRet :: PureCont -> EffCont -> Handler -> PureCont
cpsHRet k h (HRet xVar comp) x h' = do
    convComp <- cps comp k h -- h'?
    return $ CPSLet xVar x convComp


cpsHOps :: PureCont -> EffCont -> Handler -> EffCont
cpsHOps k h ops (CPair (CLabel l) (CPair p r)) =
    case hop l ops of
        Just (AlgOp _ pvar rvar comp) -> do
            contComp <- cps comp k h
            return $ CPSLet pvar p (CPSLet rvar r contComp)
        Nothing -> undefined -- todo


-- forward :: Label -> Var -> Var -> PureCont -> EffCont -> CPSMonad ContComp
-- forward y p r k h = do
--     fnvar <- freshVar
--     x <- freshVar
--     lambdaComp <- CApp r [x, k, h]
--     contComp <- h (CPair (CLabel y) (CPair p (CVar fnvar)))


runCPS :: Comp -> Either Error ContComp
runCPS e = evalState (runExceptT cpsTerm) initialState
    where cpsTerm = cps e initialPureCont initialEffCont
