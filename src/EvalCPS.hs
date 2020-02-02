{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
module Eval where

import qualified Data.Map as Map
import AST
import CPS
import Types

data DValue
    = DNum Integer
    | DLambda Env FuncRecord
    | DUnit
    | DPair DValue DValue
    | DLabel Label

type Env = Map.Map Var DValue

type FuncRecord = [DValue] -> Either Error DValue

instance Show DValue where
    show (DNum n) = show n
    show (DLambda env _) = "(λ " ++ show env ++ ")"
    show  DUnit = "()"
    show (DPair l r) = "(" ++ show l ++ ", " ++ show r ++ ")"
    show (DLabel l) = "L: " ++ show l

instance Eq DValue where
    DNum n == DNum n' = n == n'
    DUnit == DUnit = True
    DPair a b == DPair a' b' = a == a' && b == b'
    DLabel l == DLabel l' = l == l'
    -- We don't want to compare functions extensionally
    DLambda env _ == DLambda env' _ = env == env'
    _ == _ = False

unboundVarErr :: String -> Either Error DValue
unboundVarErr x = Left $ EvalError $ "Unbound variable " ++ x

absurdErr :: Either Error DValue
absurdErr = Left $ EvalError "Absurd; divergent term"

boolToInt :: Bool -> Integer
boolToInt b = if b then 1 else 0

convOp :: BinaryOp -> (Integer, Integer) -> Integer
convOp BAdd = uncurry (+)
convOp BMul = uncurry (*)
convOp BDiv = uncurry div
convOp BSub = uncurry (-)
convOp BLte = boolToInt . uncurry (<=)
convOp BLt = boolToInt . uncurry (<)
convOp BGte = boolToInt . uncurry (>=)
convOp BGt = boolToInt . uncurry (>)
convOp BEq = boolToInt . uncurry (==)
convOp BNe = boolToInt . uncurry (/=)

val :: Env -> CValue -> Either Error DValue
val env e = case e of
    CVar x -> case Map.lookup x env of
        Just v -> return v
        Nothing -> unboundVarErr x
    CNum n -> return $ DNum n
    CLabel l -> return $ DLabel l
    CUnit -> return DUnit
    CPair cL cR -> do
        dL <- val env cL
        dR <- val env cR
        return $ DPair dL dR

extendEnv :: Env -> Var -> DValue -> Env
extendEnv env x v = Map.insert x v env


deconsRow :: DValue -> Maybe (Label, DValue, DValue)
deconsRow (DPair (DLabel l) (DPair v row')) = return (l, v, row')
deconsRow DUnit = Nothing


-- todo: unify with consRow !!
consRow' :: (Label, DValue) -> DValue -> DValue
consRow' (l, v) rowV = DPair (DLabel l) (DPair v rowV)


decomposeRow :: DValue -> Label -> (Maybe DValue, DValue)
decomposeRow row l = aux id row where
    aux k row = case deconsRow row of
        Just (l', v, row') ->
            if l == l' then (return v, k row')
                       else aux (k . consRow' (l', v)) row'
        Nothing -> (Nothing, k row)


eval :: Env -> ContComp -> Either Error DValue
eval env e = case e of
    CPSValue v -> val env v
    CPSApp fE args -> val env fE >>= \f -> case f of
        DLambda env' g -> mapM (val env) args >>= g -- do we need env'?
        _ -> Left $ EvalError $ "Application of non-lambda term: " ++ show f ++ " " ++ show args
    CPSResume fvar cont -> do
        contRes <- eval env cont -- TODO: Not tail recursive!!
        val env fvar >>= \case
            DLambda env' g -> g [contRes]
    CPSBinOp op l r x cont -> do
        lv <- val env l
        rv <- val env r
        case (lv, rv) of
          (DNum n1, DNum n2) -> eval extEnv cont
              where res = DNum $ convOp op (n1, n2)
                    extEnv = extendEnv env x res
          _ -> Left $ EvalError $ "Could not perform " ++ show lv ++ " " ++ show op ++ " " ++ show rv
    CPSFix f formalParams body cont -> eval contEnv cont
        where contEnv = extendEnv env f func
              func = DLambda env funcRecord
              funcRecord actualParams =
                let params = zip formalParams actualParams in
                let extEnv = foldl (uncurry . extendEnv) env params in
                eval extEnv body
    CPSLet x varVal cont -> do
        varDVal <- val env varVal
        let env' = extendEnv env x varDVal
        eval env' cont
    CPSSplit l x y row cont -> do
        dRow <- val env row
        case decomposeRow dRow l of
          (Nothing, _) -> Left $ EvalError "Splitting non-existing label"
          (Just dv, rowRest) ->
            let env' = extendEnv env x dv in
            let env'' = extendEnv env' y rowRest in
            eval env'' cont
    CPSCase variant l x tCont y fCont -> do
        dvariant <- val env variant
        let (DPair (DLabel l') dv) = dvariant
        if l == l' then eval (extendEnv env x dv) tCont
        else eval (extendEnv env y dvariant) fCont
    CPSIf cond tCont fCont -> do
        (DNum n) <- val env cond
        eval env (if n > 0 then tCont else fCont)
    CPSAbsurd _ -> absurdErr


runEval :: ContComp -> Either Error DValue
runEval = eval Map.empty
