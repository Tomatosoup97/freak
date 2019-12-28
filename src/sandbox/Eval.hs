module Eval where

import qualified Data.Map as Map
import AST
import CPS

data DValue
    = DNum Integer
    | DLambda Env FuncRecord
    | DRecordRow (RecordRow DValue)
    | DVariantRow (VariantRow DValue)

type Env = Map.Map Var DValue

type FuncRecord = [DValue] -> Either Error DValue

instance Show DValue where
    show (DNum n) = "DNum " ++ show n
    show (DLambda _ _) = "DLambda"

convOp :: BinaryOp -> Integer -> Integer -> Integer
convOp BAdd n1 n2 = n1 + n2
convOp BMul n1 n2 = n1 * n2

val :: Env -> CValue -> Either Error DValue
val env e = case e of
    CVar x -> case Map.lookup x env of
        Just v -> return v
        Nothing -> Left $ EvalError $ "Unbound variable " ++ x
    CNum n -> return $ DNum n
    CRecordRow RecordRowUnit -> return $ DRecordRow RecordRowUnit
    CRecordRow (RecordRowExtend l cv r) -> do
        dv <- val env cv
        dRow <- val env (CRecordRow r)
        case dRow of
          DRecordRow dRow -> return $ DRecordRow (RecordRowExtend l dv dRow)
          _ -> Left $ EvalError $ "Row tail should also be row"
    CVariantRow (VariantRow t l cv) ->
        val env cv >>= (return . DVariantRow . (VariantRow t l))

extendEnv :: Env -> Var -> DValue -> Env
extendEnv env x v = Map.insert x v env

eval :: Env -> CExp -> Either Error DValue
eval env e = case e of
    CPSValue v -> val env v
    CPSApp fE args -> val env fE >>= \v -> case v of
        DLambda env' g -> mapM (val env) args >>= g -- do we need env'?
        _ -> Left $ EvalError "Application of non-lambda term"
    CPSBinOp op l r x cont -> do
        lv <- val env l
        rv <- val env r
        case (lv, rv) of
          (DNum n1, DNum n2) -> eval extEnv cont
              where res = DNum $ convOp op n1 n2
                    extEnv = extendEnv env x res
          _ -> Left $ EvalError ""
    CPSFix f formalParams body cont -> eval contEnv cont
        where contEnv = extendEnv env f func
              func = DLambda env funcRecord
              funcRecord = \actualParams ->
                let params = zip formalParams actualParams in
                let extEnv = foldl (uncurry . extendEnv) env params in
                eval extEnv body
    CPSLet x varVal cont -> do
        varDVal <- val env varVal
        let env' = extendEnv env x varDVal
        eval env' cont
