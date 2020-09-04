module AlphaConv where

import qualified Data.Map as Map
import Types
import AST
import Control.Monad.State.Lazy
import Control.Monad.Except

freshAlphaVar :: Var -> ExceptT Error (State Int) Var
freshAlphaVar s = do
    n <- lift get
    lift $ put (n+1)
    return $ toAlphaVar n s

toAlphaVar :: Int -> Var -> Var
toAlphaVar n s = "!alpha" ++ show n ++ "_" ++ s

type VarMap = Map.Map Var Var

_alphaConvV :: VarMap -> Value -> ExceptT Error (State Int) Value
_alphaConvV vmap v = case v of
    VVar x -> case Map.lookup x vmap of
        Just x' -> return $ VVar x'
        Nothing -> throwError $ unboundVarErr x
    VNum n -> return v
    VStr s -> return v
    VLambda x vType c -> do
        x' <- freshAlphaVar x
        let vmap' = Map.insert x x' vmap
        VLambda x' vType <$> _alphaConv vmap' c
    VUnit -> return v
    VPair vL vR -> do
        vL' <- alphaV vL
        VPair vL' <$> alphaV vR
    VBinOp op vL vR -> do
        vL' <- alphaV vL
        VBinOp op vL' <$> alphaV vR
    VFst v -> VFst <$> alphaV v
    VSnd v -> VSnd <$> alphaV v
    where alphaV = _alphaConvV vmap

_alphaConvH :: VarMap -> Handler -> ExceptT Error (State Int) Handler
_alphaConvH vmap (HRet x c) = do
    x' <- freshAlphaVar x
    let vmap' = Map.insert x x' vmap
    HRet x' <$> _alphaConv vmap' c

_alphaConvH vmap (HOps (AlgOp l p r c) h) = do
    r' <- freshAlphaVar r
    p' <- freshAlphaVar p
    let vmap' = Map.insert r r' (Map.insert p p' vmap)
    c' <- _alphaConv vmap' c
    HOps (AlgOp l p' r' c') <$> _alphaConvH vmap h

_alphaConv :: VarMap -> Comp -> ExceptT Error (State Int) Comp
_alphaConv vmap c = case c of
    EVal v -> EVal <$> alphaV v
    ELet x vC bC -> do
        x' <- freshAlphaVar x
        let vmap' = Map.insert x x' vmap
        vC' <- _alphaConv vmap vC
        bC' <- _alphaConv vmap' bC
        return $ ELet x' vC' bC'
    EApp f arg -> do
        f' <- alphaV f
        arg' <- alphaV arg
        return $ EApp f' arg'
    EReturn v -> EReturn <$> alphaV v
    EAbsurd v -> EAbsurd <$> alphaV v
    EIf condV cT cF -> do
        cT' <- alpha cT
        cF' <- alpha cF
        convV' <- alphaV condV
        return $ EIf convV' cT' cF'
    EOp l v -> EOp l <$> alphaV v
    EHandle c h -> do
        c' <- alpha c
        EHandle c' <$> alphaH h
    ECoop l v -> ECoop l <$> alphaV v
    ECohandle c h -> do
        c' <- alpha c
        ECohandle c' <$> alphaH h
    ECohandleIR algT v c h -> do
        v' <- alphaV v
        c' <- alpha c
        ECohandleIR algT v' c' <$> alphaH h
    where alpha = _alphaConv vmap
          alphaV = _alphaConvV vmap
          alphaH = _alphaConvH vmap

alphaConvert :: Comp -> Either Error Comp
alphaConvert c = evalState (runExceptT (_alphaConv Map.empty c)) 0
