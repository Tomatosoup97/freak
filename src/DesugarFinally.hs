module DesugarFinally where

import AST
import Types
import Parser

-- TODO: abstract walking on Comp

desugarFinallyV :: Value -> Value
desugarFinallyV v = case v of
    VLambda x t c -> VLambda x t (desugarFinally c)
    VFix g x c -> VFix g x (desugarFinally c)
    VPair v1 v2 -> VPair (desugarFinallyV v1) (desugarFinallyV v2)
    VBinOp op v1 v2 -> VBinOp op (desugarFinallyV v1) (desugarFinallyV v2)
    VVar _ -> v
    VNum _ -> v
    VStr _ -> v
    VUnit -> v
    VFst v -> VFst (desugarFinallyV v)
    VSnd v -> VSnd (desugarFinallyV v)

desugarFinallyH :: Handler -> Handler
desugarFinallyH (HRet v c) = HRet v (desugarFinally c)
desugarFinallyH (HOps (AlgOp l p r c) h) =
    HOps (AlgOp l p r (desugarFinally c)) (desugarFinallyH h)

desugarFinally :: Comp -> Comp
desugarFinally c = case c of
    EVal v -> EVal (desugarFinallyV v)
    ELet x varC bC -> ELet x (desugarFinally varC) (desugarFinally bC)
    EApp v1 v2 -> EApp (desugarFinallyV v1) (desugarFinallyV v2)
    EReturn v -> EReturn (desugarFinallyV v)
    EAbsurd v -> EAbsurd (desugarFinallyV v)
    EIf v tC fC -> EIf (desugarFinallyV v) (desugarFinally tC) (desugarFinally fC)
    EOp l v -> EOp l (desugarFinallyV v)
    EHandle c h -> EHandle (desugarFinally c) (desugarFinallyH h)
    ECoop l v -> ECoop l (desugarFinallyV v)
    ECohandleIR algT bC h -> ECohandleIR algT (desugarFinally bC) (desugarFinallyH h)
    ECohandleIRFinally algT bC (FinallyC x fC) h ->
        let desugaredFC = ELet x (desugarFinally bC) (desugarFinally fC) in
        ECohandleIR algT desugaredFC (desugarFinallyH h)
