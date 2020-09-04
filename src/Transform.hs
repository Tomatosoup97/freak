module Transform where

import AST
import Types
import Parser
import qualified Data.Map as Map

type AlgSignMap = Map.Map EffLabel AlgTheoryName

getCoalgebra :: AlgTheoryName -> Comp -> Comp
getCoalgebra algT = ELet algT (ECoop (CoeffL (algT ++ "Get")) VUnit)

putCoalgebra :: AlgTheoryName -> Value -> Comp -> Comp
putCoalgebra algT = ELet algT . ECoop (CoeffL (algT ++ "Put"))

coopTransV :: AlgSignMap -> Value -> Value
coopTransV m v = case v of
    VLambda x t c -> VLambda x t (coopTrans m c)
    VFix g x c -> VFix g x (coopTrans m c)
    VPair v1 v2 -> VPair (coopTransV m v1) (coopTransV m v2)
    VBinOp op v1 v2 -> VBinOp op (coopTransV m v1) (coopTransV m v2)
    VVar _ -> v
    VNum _ -> v
    VStr _ -> v
    VUnit -> v
    VFst v -> VFst (coopTransV m v)
    VSnd v -> VSnd (coopTransV m v)

coopTransHandler :: AlgSignMap -> Handler -> Handler
coopTransHandler m (HRet v c) = HRet v (coopTrans m c)
coopTransHandler m (HOps (AlgOp l p r c) h) =
    HOps (AlgOp l p r (coopTrans m c)) (coopTransHandler m h)

coopTrans :: AlgSignMap -> Comp -> Comp
coopTrans m c = case c of
    EVal v -> EVal (coopTransV m v)
    ELet x varC bC -> ELet x (coopTrans m varC) (coopTrans m bC)
    EApp v1 v2 -> EApp (coopTransV m v1) (coopTransV m v2)
    EReturn v -> EReturn (coopTransV m v)
    EAbsurd v -> EAbsurd (coopTransV m v)
    EIf v tC fC -> EIf (coopTransV m v) (coopTrans m tC) (coopTrans m fC)
    EOp l v -> EOp l (coopTransV m v)
    EHandle c h -> EHandle (coopTrans m c) (coopTransHandler m h)
    ECoop l v -> case Map.lookup l m of
        Just algT ->
            let algTRes = algT ++ "Res" in
            let coop = ECoop l (VPair (VVar algT) (coopTransV m v)) in
            let bindConf = ELet algT (EReturn (VFst (VVar algTRes))) in
            let bindResult = ELet algTRes coop in
            let contComp = EReturn (VSnd (VVar algTRes)) in
            -- actual code execution is from left to right:
            (getCoalgebra algT . bindResult . bindConf . putCoalgebra algT (VVar algT)) contComp
        Nothing -> ECoop l (coopTransV m v)
    ECohandleIR algTheoryName initV c h ->
        let algTVar = "_" ++ algTheoryName in
        let sign = hopsL h in
        let m' = foldl (\m s -> Map.insert s algTVar m) m sign in
        let h' = coopTransHandler m' h in
        -- TODO: Read and parse from lib/coalgHandler.fk
        let stateMonadHandler = HOps (AlgOp (CoeffL (algTVar ++ "Put")) "s'" "r" (EReturn (VLambda "s" TInt (ELet "g" (EApp (VVar "r") VUnit) (EApp (VVar "g") (VVar "s'")))))) (HOps (AlgOp (CoeffL (algTVar ++ "Get")) "_" "r" (EReturn (VLambda "s" TInt (ELet "g" (EApp (VVar "r") (VVar "s")) (EApp (VVar "g") (VVar "s")))))) (HRet "x" (EReturn (VLambda "s" TInt (EReturn (VVar "x")))))) in
        let runner = leftJoinHandlers stateMonadHandler h' in
        let cohandle = (\c -> ECohandleIR algTheoryName initV c runner) in
        let coalgComp = (cohandle . coopTrans m') c in
        let runCoalgVar = algTVar ++ "Run" in
        ELet runCoalgVar coalgComp (EApp (VVar runCoalgVar) initV)

transform :: Comp -> Comp
transform = coopTrans Map.empty
