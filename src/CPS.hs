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


instance Show CValue where
    show (CNum n) = show n
    show  CUnit = "()"
    show (CPair l r) = "(" ++ show l ++ ", " ++ show r ++ ")"
    show (CLabel l) = "(L: " ++ show l ++ ")"
    show (CVar x) = show x

data ContComp
    = CPSApp CValue [CValue]
    | CPSResume CValue ContComp
    | CPSFix Var [Var] ContComp ContComp
    | CPSBinOp BinaryOp CValue CValue Var ContComp
    | CPSValue CValue
    | CPSLet Var CValue ContComp
    | CPSSplit Label Var Var CValue ContComp
    | CPSCase CValue Label Var ContComp Var ContComp
    | CPSIf CValue ContComp ContComp
    | CPSAbsurd CValue

instance Show ContComp where
    show (CPSApp c1 cs) = "(" ++ show c1 ++ " " ++ show cs ++ ")"
    show (CPSResume c1 cont) = "(" ++ show c1 ++ " (" ++ show cont ++ "))"
    show (CPSValue v) = show v
    show (CPSFix f xs c c') = "(fix " ++ show f ++ " = " ++ "λ" ++ show xs ++ " ->\n\t" ++ show c ++ "\n\twithin\n\t" ++ show c' ++ ")"
    show (CPSBinOp op vL vR x c) = show x ++ " := (" ++ show vL ++ show op ++ show vR ++ ") in " ++ show c
    show (CPSLet x v c) = "let " ++ show x ++ " = " ++ show v ++ " in " ++ show c
    show (CPSSplit l x y v c) = undefined
    show (CPSCase v l x c y c') = "case " ++ show v ++ " { " ++ show l ++ " " ++ show x ++ "->" ++ show c ++ "; " ++ show y ++ " " ++ show c'
    show (CPSIf v c c') = "if " ++ show v ++ " then " ++ show c ++ " else " ++ show c'
    show (CPSAbsurd v) = "Absurd " ++ show v

type CPSMonad a = ExceptT Error (State Int) a

type EffCont = CValue -> CPSMonad ContComp
type PureCont = CValue -> EffCont -> CPSMonad ContComp

initialPureCont :: PureCont
initialPureCont v h = (return . CPSValue) v -- todo: point-free?

initialEffCont :: EffCont
initialEffCont (CPair z _) = return $ CPSAbsurd z -- todo: "x" shouldn't be necessary

initialState :: Int
initialState = 0

freshVar' :: String -> CPSMonad Var
freshVar' s = do
    n <- get
    put (n+1)
    return $ s ++ show n

freshVar :: CPSMonad Var
freshVar = freshVar' "__m" -- todo: this should be unique!

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
    EVal (VPair v1 v2) ->
        let cont cv1 = cps (EVal v2) (\cv2 _ -> k (CPair cv1 cv2) h) in
        cps (EVal v1) cont h
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
        let cont f = cps (EVal e2) (cont' f)
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
    EIf cond tComp fComp -> do
        -- TODO: unify with CPSCase
        tCont <- cps tComp k h
        fCont <- cps fComp k h
        cps (EVal cond) (\convCond h -> return $ CPSIf convCond tCont fCont) h
    EReturn v -> cps (EVal v) k h
    EAbsurd v -> cps (EVal v) (\cv h -> return $ CPSAbsurd cv) h
    -- Algebraic effects
    EDo l v -> do
        resumption <- freshVar' "r"
        x <- freshVar' "rArg"
        contVar <- freshVar
        pureCont <- k (CVar x) h
        let lambdaComp = CPSResume (CVar contVar) pureCont
        let pair cv = CPair (CLabel l) (CPair cv (CVar resumption))
        contComp <- cps (EVal v) (\cv _ -> h $ pair cv) h
        return $ CPSFix resumption [x, contVar] lambdaComp contComp
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
        Nothing -> throwError $ CPSError "Nested handlers are not supported yet!" -- TODO


-- forward :: Label -> Var -> Var -> PureCont -> EffCont -> CPSMonad ContComp
-- forward y p r k h = do
--     fnvar <- freshVar
--     x <- freshVar
--     lambdaComp <- CApp r [x, k, h]
--     contComp <- h (CPair (CLabel y) (CPair p (CVar fnvar)))


runCPS :: Comp -> Either Error ContComp
runCPS e = evalState (runExceptT cpsTerm) initialState
    where cpsTerm = cps e initialPureCont initialEffCont
