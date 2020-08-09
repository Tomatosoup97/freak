module CPS where

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import CommonCPS
import AST
import TargetAST
import Types

type ContF = UValue -> [Cont] -> CPSMonad UComp

data Cont = Pure ContF
          | Eff ContF

instance Show Cont where
    show (Pure f) = "Pure"
    show (Eff f) = "Eff"

initialPureCont :: ContF
initialPureCont v ks = (return . UVal) v

initialEffCont :: ContF
initialEffCont (UPair (ULabel effLabel) (UPair p r)) ks
    | effLabel == "Print" = do
        (lift . lift) (print p)
        resume UUnit
    | effLabel == "ReadLine" = do
        input <- (lift . lift) getLine
        resume $ UStr input
    | otherwise = return $ UTopLevelEffect effLabel p
    where resume = return . UApp (UVal r) . UVal

initialEffCont v _ = throwError $ CPSError $ "Incorrect value " ++ show v ++ " in effect continuation"

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
    VRecordRow row -> undefined -- todo: records are constructed using ExtendRow
    VUnit -> return UUnit
    VPair e1 e2 -> do
        v1 <- cpsVal e1 ks
        v2 <- cpsVal e2 ks
        return $ UPair v1 v2
    VExtendRow l v row -> notSupportedErr
    VVariantRow (VariantRow _ l v) -> notSupportedErr
    VLambda x _ body -> ULambda x <$> cps body ks
    VFix g x body -> URec g x <$> cps body ks
    VBinOp op e1 e2 -> do
        v1 <- cpsVal e1 ks
        v2 <- cpsVal e2 ks
        return $ UBinOp op v1 v2

cps :: Comp -> [Cont] -> CPSMonad UComp
cps e ks = case e of
    EVal v -> case ks of
        Pure kf:ks' -> cpsVal v ks' >>= \v -> kf v ks'
        _ -> cpsVal v ks >>= \v -> initialPureCont v ks
    EApp vF vArg -> do
        f <- cpsVal vF ks
        arg <- cpsVal vArg ks
        return $ UApp (UVal f) (UVal arg)
    -- todo: This ELet fails on Drop resumption result test
    ELet x varComp comp -> do
        let k:ks' = ks
        let cont varVal ks'' = do
            body <- cps comp (k:ks')
            return $ ULet x (UVal varVal) body
        cps varComp (Pure cont:ks')
    -- todo: This ELet fails on Simulate exceptions test
    -- ELet x varComp comp -> do
    --     let k:ks' = ks
    --     body <- cps comp ks
    --     varVal <- cps varComp ks
    --     return $ ULet x varVal body
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
    EReturn v -> cps (EVal v) ks
    EAbsurd v -> do
        v <- cpsVal v ks
        return $ UAbsurd v
    -- Algebraic effects
    EDo l v -> do
        let Pure kf:h@(Eff hf):ks' = ks
        x <- freshVar' "rArg"
        pureComp <- kf (UVar x) (h:ks')
        let resumption = ULambda x pureComp
        let pair cv = UPair (ULabel l) (UPair cv resumption)
        cv <- cpsVal v ks
        hf (pair cv) ks'
    EHandle body handler -> do
        let pureCont = cpsHRet (hret handler)
        let effCont = cpsHOps handler
        cps body (pureCont:effCont:ks)


cpsHRet :: Handler -> Cont
cpsHRet (HRet xVar comp) = Pure (\x ks -> do
    let Eff hf:ks' = ks
    convComp <- cps comp ks'
    return $ ULet xVar (UVal x) convComp)


cpsHOps :: Handler -> Cont
cpsHOps ops = Eff (\(UPair (ULabel l) (UPair p r)) ks ->
    case hop l ops of
        Just (AlgOp _ pvar rvar comp) -> do
            contComp <- cps comp ks
            return $ ULet pvar (UVal p) (ULet rvar (UVal r) contComp)
        Nothing -> forward l p r ks)


forward :: Label -> UValue -> UValue -> [Cont] -> CPSMonad UComp
forward y p r ks = do
    let k'@(Pure kf'):h'@(Eff hf'):ks' = ks
    x <- freshVar' "rArg"
    pureComp <- kf' (UVar x) (h':ks')
    let resumption = ULambda x pureComp
    let pair = UPair (ULabel y) (UPair p resumption)
    hf' pair ks'


runCPS :: Comp -> EvalResMonad UComp
runCPS e = evalStateT (runExceptT cpsTerm) initialState
    where cpsTerm = cps e [Pure initialPureCont, Eff initialEffCont]
