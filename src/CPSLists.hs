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
          | Coeff ContF

unfoldCont :: Cont -> ContF
unfoldCont (Pure c) = c
unfoldCont (Eff c) = c
unfoldCont (Coeff c) = c

instance Show Cont where
    show (Pure f) = "Pure"
    show (Eff f) = "Eff"
    show (Coeff f) = "Coeff"

instance Eq Cont where
    Pure _ == Pure _ = True
    Eff _ == Eff _ = True
    Coeff _ == Coeff _ = True
    _ == _ = False

initialPureCont :: ContF
initialPureCont v ks = (return . UVal) v

initialEffCont :: ContF
initialEffCont (UPair (ULabel effLabel) (UPair p r)) ks =
    return . UApp (UVal r) $ UTopLevelEffect effLabel p
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
    VUnit -> return UUnit
    VPair e1 e2 -> do
        v1 <- cpsVal e1 ks
        v2 <- cpsVal e2 ks
        return $ UPair v1 v2
    VBinOp op e1 e2 -> do
        v1 <- cpsVal e1 ks
        v2 <- cpsVal e2 ks
        return $ UBinOp op v1 v2
    VLambda x _ body -> ULambda x <$> cps body ks
    VFix g x body -> URec g x <$> cps body ks
    VRecordRow row -> undefined -- todo: records are constructed using ExtendRow
    VExtendRow l v row -> notSupportedErr
    VVariantRow (VariantRow _ l v) -> notSupportedErr

cps :: Comp -> [Cont] -> CPSMonad UComp
cps e ks = case e of
    EVal v -> cpsVal v ks >>= (return . UVal)
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
    EReturn v -> case ks of
        Pure kf:ks' -> cpsVal v ks' >>= \v -> kf v ks'
        [Eff _] -> cpsVal v ks >>= \v -> initialPureCont v ks
    EAbsurd v -> do
        v <- cpsVal v ks
        return $ UAbsurd v
    -- Algebraic effects
    EOp l v -> case ks of
        (Pure kf:(Eff hf):ks') -> cpsOp l v ks
        (Pure kf:(Coeff hf):ks') -> throwError $ CPSError "Effect cannot go pass cohandler!"
        _ -> throwError $ CPSError $ "Ildefined stack of continuations: " ++ show ks
    EHandle body handler ->
        cpsHandle body handler Eff ks
    -- Coalgebraic effects
    ECohandle body handler ->
        cpsHandle body handler Coeff ks
    ECoop l v -> case ks of
        (Pure kf:h:ks') -> cpsOp l v ks
        _ -> throwError $ CPSError $ "Ildefined stack of continuations: " ++ show ks


cpsHandle :: Comp -> Handler -> (ContF -> Cont) -> [Cont] -> CPSMonad UComp
cpsHandle body handler effCons ks = do
    let pureCont = cpsHRet (hret handler)
    let effCont = cpsHOps effCons handler
    cps body (pureCont:effCont:ks)


cpsOp :: Label -> Value -> [Cont] -> CPSMonad UComp
cpsOp l v ks = do
    let Pure kf:h:ks' = ks
    let hf = unfoldCont h
    x <- freshVar' "rArg"
    pureComp <- kf (UVar x) (h:ks')
    let resumption = ULambda x pureComp
    let pair cv = UPair (ULabel l) (UPair cv resumption)
    cv <- cpsVal v ks
    hf (pair cv) ks'


cpsHRet :: Handler -> Cont
cpsHRet (HRet xVar comp) = Pure (\x (_:ks') -> do
    convComp <- cps comp ks'
    return $ ULet xVar (UVal x) convComp)


cpsHOps :: (ContF -> Cont) -> Handler -> Cont
cpsHOps effCons ops = effCons (\(UPair (ULabel l) (UPair p r)) ks ->
    case hop l ops of
        Just (AlgOp _ pvar rvar comp) -> do
            contComp <- cps comp ks
            return $ ULet pvar (UVal p) (ULet rvar (UVal r) contComp)
        Nothing -> forward l p r ks)


forward :: Label -> UValue -> UValue -> [Cont] -> CPSMonad UComp
forward y p r ks = do
    -- TODO: forwarding should not let effects go pass cohandler
    let k'@(Pure kf'):h':ks' = ks
    let hf' = unfoldCont h'
    x <- freshVar' "rArg"
    pureComp <- kf' (UVar x) (h':ks')
    let resumption = ULambda x pureComp
    let pair = UPair (ULabel y) (UPair p resumption)
    hf' pair ks'


runCPS :: Comp -> EvalResMonad UComp
runCPS e = evalStateT (runExceptT cpsTerm) initialState
    where cpsTerm = cps e [Pure initialPureCont, Eff initialEffCont]
