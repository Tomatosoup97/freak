module StaticAnalyzer where

import AST
import Types
import Parser

restrictEffectsV :: Bool -> Value -> Either Error Value
restrictEffectsV b v = case v of
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
    where rec = restrictEffects b
          recV = restrictEffectsV b

restrictEffectsH :: Bool -> Handler -> Either Error Handler
restrictEffectsH b (HRet v c) = HRet v <$> restrictEffects b c
restrictEffectsH b (HOps (AlgOp l p r c) h) = do
    c' <- restrictEffects b c
    HOps (AlgOp l p r c') <$> restrictEffectsH b h

restrictEffectsFC :: Bool -> FinallyClause -> Either Error FinallyClause
restrictEffectsFC b (FinallyC x fC) = FinallyC x <$> restrictEffects b fC

restrictEffects :: Bool -> Comp -> Either Error Comp
restrictEffects b c = case c of
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
        if b then Left effectInFinallyErr
        else EOp l <$> recV v
    EHandle c h -> do
        c' <- rec c
        EHandle c' <$> recH h
    ECoop l v -> ECoop l <$> recV v
    ECohandleIR algT bC h -> do
        bC' <- rec bC
        ECohandleIR algT bC' <$> recH h
    ECohandleIRFinally algT bC fC h -> do
        bC' <- rec bC
        fC' <- restrictEffectsFC True fC
        ECohandleIRFinally algT bC' fC' <$> recH h
    where rec = restrictEffects b
          recV = restrictEffectsV b
          recH = restrictEffectsH b

staticAnalyze :: Comp -> Either Error Comp
staticAnalyze = restrictEffects False
