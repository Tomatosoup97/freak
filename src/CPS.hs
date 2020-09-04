module CPS where

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import CPSMonad
import AST
import TargetAST
import Types

showCont :: Cont -> CPSMonad ()
showCont k = case k of
    Pure f -> printCont f "Pure cont"
    Eff f -> printCont f "Eff cont"
    Coeff f -> printCont f "Coeff cont"
    where printCont f repr = do
            v <- freshVar' repr
            (liftIO . putStrLn) $ "-- Start " ++ v
            res <- f (UVar "[-]") (cycle [Pure initialPureCont, Eff initialEffCont])
            (liftIO . putStrLn) $ "-- End Pure " ++ v ++ " " ++ show res

initialPureCont :: ContF
initialPureCont v ks = (return . UVal) v

initialEffCont :: ContF
initialEffCont (UPair (UEffLabel l) (UPair p r)) ks =
    return $ UApp (UVal r) (UTopLevelEffect l p) ks
initialEffCont v _ = throwError $ CPSError $ "Incorrect value " ++ show v ++ " in effect continuation"

initialContStack :: [Cont]
initialContStack = [Pure initialPureCont, Eff initialEffCont]

initialState :: Int
initialState = 0

consRow :: (Label, UValue) -> UValue -> UValue
consRow (l, v) rowV = UPair (ULabel l) (UPair v rowV)

jota :: Label -> UValue -> UValue
jota l = UPair (ULabel l)

notSupportedErr :: CPSMonad UValue
notSupportedErr = throwError $ CPSError "The operation is not supported yet"

cpsVal :: Value -> [Cont] -> CPSMonad UValue
cpsVal e ks = case e of
    VVar x -> return $ UVar x
    VNum n -> return $ UNum n
    VStr s -> return $ UStr s  -- todo: write about support for strings
    VUnit -> return UUnit
    VPair e1 e2 -> do
        v1 <- cpsVal e1 ks
        v2 <- cpsVal e2 ks
        return $ UPair v1 v2
    VFst e -> UFst <$> cpsVal e ks
    VSnd e -> USnd <$> cpsVal e ks
    VBinOp op e1 e2 -> do
        v1 <- cpsVal e1 ks
        v2 <- cpsVal e2 ks
        return $ UBinOp op v1 v2
    VLambda x _ body -> return $ ULambda x (cps body)
    VFix g x body -> URec g x <$> cps body ks
    VRecordRow row -> undefined -- todo: records are constructed using ExtendRow
    VExtendRow l v row -> notSupportedErr
    VVariantRow (VariantRow _ l v) -> notSupportedErr

cps :: Comp -> [Cont] -> CPSMonad UComp
cps e ks = case e of
    EVal v -> cpsVal v ks >>= (return . UVal)
    EApp vF vArg -> do
        let k@(Pure kf):ks' = ks
        f <- cpsVal vF ks
        arg <- cpsVal vArg ks
        return $ UApp (UVal f) arg ks
    ELet x varComp comp -> do
        let k:ks' = ks
        let cont varVal ks'' = do
            body <- cps comp (k:ks'')
            return $ ULet x varVal body
        cps varComp (Pure cont:ks')
    ESplit l x y row comp -> do
        v <- cpsVal row ks
        c <- cps comp ks
        return $ USplit l x y v c
    ECase variant l x tComp y fComp -> do
        v <- cpsVal variant ks
        tC <- cps tComp ks
        fC <- cps fComp ks
        return $ UCase v l tC y fC
    EIf cond tComp fComp -> do
        cond <- cpsVal cond ks
        tC <- cps tComp ks
        fC <- cps fComp ks
        return $ UIf cond tC fC
    EReturn v -> case ks of
        Pure kf:ks' -> cpsVal v ks' >>= \v -> kf v ks'
        _ -> throwError $ CPSError $ "Illdefined stack of continuations when handling  " ++ show v
    EAbsurd v -> do
        v <- cpsVal v ks
        return $ UAbsurd v
    -- Algebraic effects
    EOp l v -> cpsOp l v ks
    EHandle body handler ->
        cpsHandle body handler EffT ks
    -- Coalgebraic effects
    ECohandle body handler       -> cpsHandle body handler CoeffT ks
    ECohandleIR _ _ body handler -> cpsHandle body handler CoeffT ks
    ECoop l v -> cpsOp l v ks


cpsHandle :: Comp -> Handler -> EffT -> [Cont] -> CPSMonad UComp
cpsHandle body handler effT ks = do
    let pureCont = cpsHRet (hret handler)
    let effCont = cpsHOps effT handler
    cps body (pureCont:effCont:ks)


cpsOp :: EffLabel -> Value -> [Cont] -> CPSMonad UComp
cpsOp l v ks = do
    let Pure kf:h:ks' = ks
    let hf = unfoldCont h
    x <- freshVar' "rArgOp"
    let resF ks = kf (UVar x) (h:ks)
    let resumption = ULambda x resF
    cv <- cpsVal v ks
    let pair = UPair (UEffLabel l) (UPair cv resumption)
    hf pair ks'


cpsHRet :: Handler -> Cont
cpsHRet (HRet xVar comp) = Pure (\x (_:ks') -> do
    convComp <- cps comp ks'
    return $ ULet xVar x convComp)


cpsHOps :: EffT -> Handler -> Cont
cpsHOps effT ops = getEffCons effT (\(UPair (UEffLabel l) (UPair p r)) ks ->
    case (l, effT) of
        (EffL l, CoeffT) -> throwError $ CPSError "Effect cannot go pass cohandler!"
        _ -> case hop l ops of
            Just (AlgOp _ pvar rvar comp) -> do
                contComp <- cps comp ks
                return $ ULet pvar p (ULet rvar r contComp)
            Nothing -> forward l p r ks)


forward :: EffLabel -> UValue -> UValue -> [Cont] -> CPSMonad UComp
forward y p r ks = do
    let k'@(Pure kf'):h':ks' = ks
    let hf' = unfoldCont h'
    x <- freshVar' "rArg"
    let resF ks'' = kf' (UVar x) (h':ks'')
    let resumption = ULambda x resF
    let pair = UPair (UEffLabel y) (UPair p resumption)
    hf' pair ks'


runCPS :: Comp -> EvalResMonad UComp
runCPS e = evalStateT (runExceptT cpsTerm) initialState
    where cpsTerm = cps e initialContStack
