module Transform where

import AST

coopTransV :: Value -> Value
coopTransV v = case v of
    VLambda x t c -> VLambda x t (coopTrans c)
    VFix g x c -> VFix g x (coopTrans c)
    VPair v1 v2 -> VPair (coopTransV v1) (coopTransV v2)
    VBinOp op v1 v2 -> VBinOp op (coopTransV v1) (coopTransV v2)
    v -> v

coopTransHandler :: Handler -> Handler
coopTransHandler (HRet v c) = HRet v (coopTrans c)
coopTransHandler (HOps (AlgOp l p r c) h) =
    HOps (AlgOp l p r (coopTrans c)) (coopTransHandler h)

coopTrans :: Comp -> Comp
coopTrans c = case c of
    EVal v -> EVal (coopTransV v)
    ELet x varC bC -> ELet x (coopTrans varC) (coopTrans bC)
    EApp v1 v2 -> EApp (coopTransV v1) (coopTransV v2)
    EReturn v -> EReturn (coopTransV v)
    EAbsurd v -> EAbsurd (coopTransV v)
    EIf v tC fC -> EIf (coopTransV v) (coopTrans tC) (coopTrans fC)
    EOp l v -> EOp l (coopTransV v)
    EHandle c h -> EHandle (coopTrans c) (coopTransHandler h)
    ECoop l v -> ECoop l (coopTransV v)
    ECohandleIR algTheoryName initV c h -> ECohandle (coopTrans c) (coopTransHandler h)

transform :: Comp -> Comp
transform = coopTrans
