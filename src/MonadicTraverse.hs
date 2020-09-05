module MonadicTraverse where

import AST
import Types

data AstFold = AstF (Value -> Either Error Value)
                    (Comp -> Either Error Comp)
                    (Handler -> Either Error Handler)

type AstTraverser a = AstFold -> a -> Either Error a

astTraverserV :: AstTraverser Value
astTraverserV g@(AstF gV _ _) v = case v of
    VLambda x t c -> rec c >>= gV . VLambda x t
    VFix g x c -> rec c >>= gV . VFix g x
    VPair vL vR -> do
        vL' <- recV vL
        vR' <- recV vR
        gV $ VPair vL' vR'
    VBinOp op vL vR -> do
        vL' <- recV vL
        vR' <- recV vR
        gV $ VBinOp op vL' vR'
    VVar _ -> gV v
    VNum _ -> gV v
    VStr _ -> gV v
    VUnit -> gV v
    VFst v -> recV v >>= gV . VFst
    VSnd v -> recV v >>= gV .  VSnd
    where rec = astTraverser g
          recV = astTraverserV g

astTraverserH :: AstTraverser Handler
astTraverserH g@(AstF _ _ gH) h = case h of
    HRet v c -> astTraverser g c >>= gH . HRet v
    HOps (AlgOp l p r c) h -> do
        c' <- astTraverser g c
        h' <- astTraverserH g h
        gH $ HOps (AlgOp l p r c') h'

astTraverserFC :: AstTraverser FinallyClause
astTraverserFC g (FinallyC x fC) = FinallyC x <$> astTraverser g fC

astTraverser :: AstTraverser Comp
astTraverser g@(AstF _ gC _) c = case c of
    EVal v ->  recV v >>= gC . EVal
    ELet x varC bC -> do
        varC' <- rec varC
        bC' <- rec bC
        gC $ ELet x varC' bC'
    EApp v1 v2 -> do
        v1' <- recV v1
        v2' <- recV v2
        gC $ EApp v1' v2'
    EReturn v -> recV v >>= gC . EReturn
    EAbsurd v -> recV v >>= gC . EAbsurd
    EIf v tC fC -> do
        v' <- recV v
        tC' <- rec tC
        fC' <- rec fC
        gC $ EIf v' tC' fC'
    EOp l v -> recV v >>= gC . EOp l
    EHandle c h -> do
        c' <- rec c
        h' <- recH h
        gC $ EHandle c' h'
    ECoop l v -> recV v >>= gC . ECoop l
    ECohandleIR algT bC h -> do
        bC' <- rec bC
        h' <- recH h
        gC $ ECohandleIR algT bC' h'
    ECohandleIRFinally algT bC fC h -> do
        bC' <- rec bC
        fC' <- recFC fC
        h' <- recH h
        gC $ ECohandleIRFinally algT bC' fC' h'
    where rec = astTraverser g
          recV = astTraverserV g
          recH = astTraverserH g
          recFC = astTraverserFC g

astTraverse :: Comp -> Either Error Comp
astTraverse = astTraverser (AstF return return return)
