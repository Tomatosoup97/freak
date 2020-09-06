module EffectsRestricter where

import AST
import Types
import Parser

_restrictEffsV :: Bool -> Value -> Either Error Value
_restrictEffsV b v = case v of
    VLambda x t c -> VLambda x t <$> rec c
    VFix g x c -> VFix g x <$> rec c
    VPair vL vR -> do
        vL' <- recV vL
        VPair vL' <$> recV vR
    VBinOp op vL vR -> do
        vL' <- recV vL
        VBinOp op vL' <$> recV vR
    VVar _ -> return v
    VNum _ -> return v
    VStr _ -> return v
    VUnit -> return v
    VFst v -> VFst <$> recV v
    VSnd v -> VSnd <$> recV v
    where rec = _restrictEffs b
          recV = _restrictEffsV b

_restrictEffsH :: Bool -> Handler -> Either Error Handler
_restrictEffsH b (HRet v c) = HRet v <$> _restrictEffs b c
_restrictEffsH b (HOps (AlgOp l p r c) h) = do
    c' <- _restrictEffs b c
    HOps (AlgOp l p r c') <$> _restrictEffsH b h

_restrictEffsFC :: Bool -> FinallyClause -> Either Error FinallyClause
_restrictEffsFC b (FinallyC x fC) = FinallyC x <$> _restrictEffs b fC

_restrictEffs :: Bool -> Comp -> Either Error Comp
_restrictEffs b c = case c of
    EVal v -> EVal <$> recV v
    ELet x varC bC -> do
        varC' <- rec varC
        ELet x varC' <$> rec bC
    EApp v1 v2 -> do
        v1' <- recV v1
        EApp v1' <$> recV v2
    EReturn v -> EReturn <$> recV v
    EAbsurd v -> EAbsurd <$> recV v
    EIf v tC fC -> do
        v' <- recV v
        tC' <- rec tC
        EIf v' tC' <$> rec fC
    EOp l v ->
        if b then Left invalidUseOfEffErr
        else EOp l <$> recV v
    EHandle c h -> do
        c' <- rec c
        EHandle c' <$> recH h
    ECoop l v -> ECoop l <$> recV v
    ECohandleIR algT bC h -> do
        bC' <- rec bC
        h' <- _restrictEffsH True h
        return $ ECohandleIR algT bC' h'
    ECohandleIRFinally algT bC fC h -> do
        bC' <- rec bC
        fC' <- _restrictEffsFC True fC
        h' <- _restrictEffsH True h
        return $ ECohandleIRFinally algT bC' fC' h'
    where rec = _restrictEffs b
          recV = _restrictEffsV b
          recH = _restrictEffsH b

restrictEffects :: Comp -> Either Error Comp
restrictEffects = _restrictEffs False
