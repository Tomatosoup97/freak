module TargetEval where

import TargetAST
import qualified Data.Map as Map

type Env = Map.Map UVar UValue

data Error = EvalError String
    deriving (Show)


opEval :: Num a => UBinaryOp -> (a -> a -> a)
opEval UAdd = (+)
opEval UMul = (*)


eval :: Env -> UComp -> Either Error UValue
eval env (USplit x y (UPairr lv rv) comp) =
    let extEnv = Map.insert y rv (Map.insert x lv env) in
    eval extEnv comp

eval env (USplit x y _ c) = Left $ EvalError "Splitting non-pair argument"

eval env (UApp fnComp arg) = do
    fnVal <- eval env fnComp -- TODO: evaluation context?
    case fnVal of
        ULambda x body -> eval (Map.insert x arg env) body
        _ -> Left $ EvalError "Application of non-lambda term"

eval env (UExpr (UBinOp op e1 e2)) = do
    (UNum n1) <- eval env (UExpr e1)
    (UNum n2) <- eval env (UExpr e2)
    return $ UNum $ (opEval op) n1 n2

eval env (UExpr (UVal v)) = return v

runEval :: UComp -> Either Error UValue
runEval = eval Map.empty
