module TargetAST where

import Types
import AST


data UValue
    = UVar Var
    | UNum Integer
    | UStr String
    | UBool Bool
    | ULambda Var UComp
    | UUnit
    | UPair UValue UValue
    | ULabel Label
    | URec Var Var UComp
    | UBinOp BinaryOp UValue UValue
    deriving (Eq)

data UComp
    = UVal UValue
    | UApp UComp UComp
    | USplit Label Var Var UValue UComp
    | UCase UValue Label UComp Var UComp
    | UIf UValue UComp UComp
    | ULet Var UComp UComp
    | UAbsurd UValue
    | UTopLevelEffect Label UValue
    deriving (Eq)

parens :: String -> String
parens s = "(" ++ s ++ ")"

instance Show UValue where
    show (UBinOp op vL vR) = parens $ show vL ++ show op ++ show vR
    show (UNum n) = show n
    show (UStr s) = "\"" ++ show s ++ "\""
    show  UUnit = "()"
    show (UPair l r) = parens $ show l ++ ", " ++ show r
    show (ULabel l) = parens $ "L: " ++ show l
    show (UVar x) = show x
    show (ULambda x c) = parens $ "\\" ++ show x ++ " -> " ++ show c

instance Show UComp where
    show (UVal v) = show v
    show (UApp f arg) = parens $ show f ++ " " ++ show arg
    show (USplit l x y v c) = undefined
    -- show (UCase v l x c y c') = "case " ++ show v ++ " { " ++ show l ++ " " ++ show x ++ "->" ++ show c ++ "; " ++ show y ++ " " ++ show c'
    show (UIf v c c') = "if " ++ show v ++ " then " ++ show c ++ " else " ++ show c'
    show (UAbsurd v) = "absurd " ++ show v
    show (ULet x xComp comp) = parens $ "let " ++ show x ++ " = " ++ show xComp ++ " in " ++ show comp
    show (UTopLevelEffect l p) = show l ++ show p
