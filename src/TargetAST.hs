module TargetAST where

import Types
import AST
import CPSMonad

type ContF = UValue -> [Cont] -> CPSMonad UComp

data Cont = Pure ContF
          | Eff ContF
          | Coeff ContF

instance Show Cont where
    show (Pure f) = "Pure"
    show (Eff f) = "Eff"
    show (Coeff f) = "Coeff"

instance Eq Cont where
    Pure _ == Pure _ = True
    Eff _ == Eff _ = True
    Coeff _ == Coeff _ = True
    _ == _ = False

data EffT = EffT | CoeffT

getEffCons :: EffT -> (ContF -> Cont)
getEffCons EffT = Eff
getEffCons CoeffT = Coeff

unfoldCont :: Cont -> ContF
unfoldCont (Pure c) = c
unfoldCont (Eff c) = c
unfoldCont (Coeff c) = c

data UValue
    = UVar Var
    | UNum Integer
    | UStr String
    | UBool Bool
    | ULambda Var ([Cont] -> CPSMonad UComp)
    | UUnit
    | UPair UValue UValue
    | ULabel Label
    | UEffLabel EffLabel
    | URec Var Var UComp
    | UBinOp BinaryOp UValue UValue
    | UFst UValue
    | USnd UValue
    -- deriving (Eq)

data UComp
    = UVal UValue
    | UApp UComp UComp [Cont]
    | USplit Label Var Var UValue UComp
    | UCase UValue Label UComp Var UComp
    | UIf UValue UComp UComp
    | ULet Var UComp UComp
    | UAbsurd UValue
    | UTopLevelEffect EffLabel UValue
    -- deriving (Eq)

instance Show UValue where
    show (UBinOp op vL vR) = inParens $ show vL ++ show op ++ show vR
    show (UNum n) = show n
    show (UStr s) = s
    show  UUnit = "()"
    show (UPair l r) = inParens $ show l ++ ", " ++ show r
    show (ULabel l) = inParens $ "L: " ++ l
    show (UEffLabel (EffL l)) = inParens $ "Eff: " ++ l
    show (UEffLabel (CoeffL l)) = inParens $ "Coeff: " ++ l
    show (UVar x) = x
    show (ULambda x _) = inParens $ x ++ " -> comp"
    show (UFst v) = "fst " ++ show v
    show (USnd v) = "snd " ++ show v

instance Show UComp where
    show (UVal v) = show v
    show (UApp f arg _) = inParens $ show f ++ " " ++ show arg
    show (USplit l x y v c) = undefined
    -- show (UCase v l x c y c') = "case " ++ show v ++ " { " ++ show l ++ " " ++ show x ++ "->" ++ show c ++ "; " ++ show y ++ " " ++ show c'
    show (UIf v c c') = "if " ++ show v ++ " then " ++ show c ++ " else " ++ show c'
    show (UAbsurd v) = "absurd " ++ show v
    show (ULet x xComp comp) = inParens $ "let " ++ x ++ " = " ++ show xComp ++ " in " ++ show comp
    show (UTopLevelEffect l p) = show l ++ show p
