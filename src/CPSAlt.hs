module CPSAlt where

import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import AST
import TargetAST
import Types
import Debug.Trace

debug = flip trace

type CPSMonad a = ExceptT Error (State Int) a

type EffCont = UValue -> CPSMonad UComp
type PureCont = UValue -> EffCont -> CPSMonad UComp

initialPureCont :: PureCont
initialPureCont v h = (return . UVal) v -- todo: point-free?

initialEffCont :: EffCont
initialEffCont (UPair z _) = return $ UAbsurd z

initialState :: Int
initialState = 0

freshVar' :: String -> CPSMonad Var
freshVar' s = do
    n <- get
    put (n+1)
    return $ s ++ show n

freshVar :: CPSMonad Var
freshVar = freshVar' "__m" -- todo: this should be unique!

consRow :: (Label, UValue) -> UValue -> UValue
consRow (l, v) rowV = UPair (ULabel l) (UPair v rowV)

jota :: Label -> UValue -> UValue
jota l = UPair (ULabel l)

-- notHandledYetErr :: String -> Either Error UValue
notSupportedErr = throwError $ CPSError "The operation is not supported yet"

cpsVal :: Value -> PureCont -> EffCont -> CPSMonad UValue
cpsVal e k h = case e of
    VVar x -> return $ UVar x
    VNum n -> return $ UNum n
    VRecordRow row -> undefined -- todo: records are constructed using ExtendRow
    VUnit -> return UUnit
    VPair e1 e2 -> do
        v1 <- cpsVal e1 k h
        v2 <- cpsVal e2 k h
        return $ UPair v1 v2
    VExtendRow l v row -> notSupportedErr
    VVariantRow (VariantRow _ l v) -> notSupportedErr
    VLambda x _ body -> ULambda x <$> cps body k h `debug` ("lambda " ++ show x)
    VFix g x body -> URec g x <$> cps body k h
    VBinOp op e1 e2 -> do
        v1 <- cpsVal e1 k h
        v2 <- cpsVal e2 k h
        return $ UBinOp op v1 v2

cps :: Comp -> PureCont -> EffCont -> CPSMonad UComp
cps e k h = case e of
    EVal v -> cpsVal v initialPureCont h >>= \v -> k v h
    -- EVal v -> UVal <$> cpsVal v k h
    EApp vF vArg -> do
        f <- cpsVal vF k h
        arg <- cpsVal vArg k h
        return $ UApp (UVal f) (UVal arg)
    ELet x varComp comp -> do
        -- varVal <- cps varComp k h
        -- body <- cps comp k h
        -- return $ ULet x varVal body
        (cps `debug` "let") varComp (
            \varVal h -> do
                body <- cps comp k h `debug` ("k called with: " ++ show varVal)
                return $ ULet x varVal body) h
    ESplit l x y row comp -> do
        v <- cpsVal row k h
        c <- cps comp k h
        return $ USplit l x y v c
    ECase variant l x tComp y fComp -> do
        v <- cpsVal variant k h
        tC <- cps tComp k h
        fC <- cps fComp k h
        return $ UCase v l tC y fC
    EIf cond tComp fComp -> do
        cond <- cpsVal cond k h
        tC <- cps tComp k h
        fC <- cps fComp k h
        return $ UIf cond tC fC
    -- EReturn v -> cps (EVal v) k h
    EReturn v -> cps (EVal v) k h `debug` ("return " ++ show v)
    EAbsurd v -> do
        v <- cpsVal v k h
        return $ UAbsurd v
    -- Algebraic effects
    EDo l v -> do
        x <- freshVar' "rArg"
        pureComp <- k (UVar x) h
        let resumption = ULambda x pureComp
        let pair cv = UPair (ULabel l) (UPair cv resumption)
        cps (EVal v) (\cv _ -> h (pair cv)) h
    EHandle body handler -> do
        let pureCont = cpsHRet k h (hret handler)
        let effCont = cpsHOps k h handler
        cps body pureCont effCont


cpsHRet :: PureCont -> EffCont -> Handler -> PureCont
cpsHRet k h (HRet xVar comp) x h' = do
    convComp <- cps comp k h
    return $ ULet xVar x convComp


cpsHOps :: PureCont -> EffCont -> Handler -> EffCont
cpsHOps k h ops (UPair (ULabel l) (UPair p r)) =
    case hop l ops of
        Just (AlgOp _ pvar rvar comp) -> do
            contComp <- cps comp k h
            return $ ULet pvar p (ULet rvar r contComp)
        Nothing -> throwError $ CPSError "Nested handlers are not supported yet!" -- TODO


runCPS :: Comp -> Either Error UComp
runCPS e = evalState (runExceptT cpsTerm) initialState
    where cpsTerm = cps e initialPureCont initialEffCont
