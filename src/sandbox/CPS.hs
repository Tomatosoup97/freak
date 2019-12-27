module CPS where

import AST
import Types

data CValue
    = CVar Var
    | CNum Integer
    deriving (Show)

data CExp
    = CPSApp CValue [CValue]
    | CPSFix Var [Var] CExp CExp
    | CPSBinOp BinaryOp CValue CValue Var CExp
    | CPSValue CValue
    -- | CPSLet Var CExp CExp
    deriving (Show)

type Cont = CValue -> CExp

initialCont :: Cont
initialCont = CPSValue

cps :: Expr -> Cont -> CExp
cps e c = case e of
    EVal (VVar x) -> c $ CVar x
    EVal (VNum n) -> c $ CNum n
    EVal (VLambda x _ body) -> CPSFix fnvar [x, contVar] convBody contExpr
        where convBody = cps body (\v -> CPSApp (CVar contVar) [v])
              fnvar = "f"
              contVar = "k"
              contExpr = c $ CVar fnvar
    EVal (VFix g x body) -> CPSFix g [x, contVar] convBody contExpr
        where convBody = cps body (\v -> CPSApp (CVar contVar) [v])
              contVar = "k"
              contExpr = c $ CVar g
    EBinOp op e1 e2 -> cps e1 (\v1 -> cps e2 (\v2 -> CPSBinOp op v1 v2 opVar contExpr))
        where opVar = "v"
              contExpr = c $ CVar opVar
    EApp e1 e2 ->
        CPSFix resVar [resArg] resBody (cps e1 (\f -> cps e2 (\v -> CPSApp f [v, CVar resVar])))
        where resVar = "r"
              resArg = "x"
              resBody = c $ CVar resArg
    -- ELet x varExpr e -> CPSLet x (cps varExpr \)
