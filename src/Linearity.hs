module Linearity where

import AST
import Types
import Parser
import qualified Data.Map as Map

type Carrier = Map.Map Var Int

-- TODO: document this feature
-- TODO: requires more extensive tests

isCarrierValid :: Carrier -> Bool
isCarrierValid = all (<2)

linUsageV :: Carrier -> Value -> Either Error Carrier
linUsageV d v = case v of
    VLambda x t c -> do
        let dEmpty = Map.map (const 0) d
        dLambda <- linUsage dEmpty c
        if isCarrierValid dLambda then return d
        else Left linearContUsageErr
    VPair vL vR -> do
        d' <- linUsageV d vL
        linUsageV d' vR
    VBinOp op vL vR -> do
        d' <- linUsageV d vL
        linUsageV d' vR
    VVar _ -> return d
    VNum _ -> return d
    VStr _ -> return d
    VUnit -> return d
    VFst v -> linUsageV d v
    VSnd v -> linUsageV d v

linUsageH :: Bool -> Carrier -> Handler -> Either Error Carrier
linUsageH b d (HRet v c) = linUsage d c
linUsageH b@False d (HOps (AlgOp l p r c) h) = do
    d' <- linUsage d c
    linUsageH b d' h
linUsageH b@True d (HOps (AlgOp l p r c) h) = do
    let dNew = Map.insert r 0 d
    d' <- linUsage dNew c
    linUsageH b d' h

linUsage :: Carrier -> Comp -> Either Error Carrier
linUsage d c = case c of
    EVal v -> linUsageV d v
    ELet x varC bC -> do
        d' <- linUsage d varC
        linUsage d' bC
    EApp v1@(VVar f) v2 ->
        case Map.lookup f d of
            Just n -> let dNew = Map.insert f (n+1) d in
                      linUsageV dNew v1 >>= \d' -> linUsageV d' v2
            Nothing -> linUsageV d v1 >>= \d' -> linUsageV d' v2
    EApp v1 v2 -> do
        d' <- linUsageV d v1
        linUsageV d' v2
    EReturn v -> linUsageV d v
    EAbsurd v -> linUsageV d v
    EIf v tC fC -> do
        d' <- linUsageV d v
        dT <- linUsage d' tC
        if isCarrierValid dT then linUsage d' fC
        else Left linearContUsageErr
    EOp l v -> linUsageV d v
    EHandle c h -> do
        d' <- linUsage d c
        if isCarrierValid d' then linUsageH False d h
        else Left linearContUsageErr
    ECoop l v -> linUsageV d v
    ECohandle bC h -> do
        dC <- linUsage d bC
        if isCarrierValid dC then linUsageH True d h
        else Left linearContUsageErr
    ECohandleIR (AlgTC _ algTV) bC h -> do
        dAlgV <- linUsageV d algTV
        dC <- linUsage dAlgV bC
        if isCarrierValid dC then linUsageH True d h
        else Left linearContUsageErr

analyzeContLinearity :: Comp -> Either Error Comp
analyzeContLinearity c = do
    d <- linUsage Map.empty c
    if isCarrierValid d then return c
    else Left linearContUsageErr
