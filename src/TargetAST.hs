module TargetAST where

import Types
import AST
import CPSMonad
import Control.Monad

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
    | UTopLevelEffect EffLabel UValue
    -- deriving (Eq)

data UComp
    = UVal UValue
    | UApp UComp UValue [Cont]
    | USplit Label Var Var UValue UComp
    | UCase UValue Label UComp Var UComp
    | UIf UValue UComp UComp
    | ULet Var UValue UComp
    | UAbsurd UValue
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
    show (ULambda x f) = inParens $ x ++ " -> comp"
    show (UFst v) = "fst " ++ show v
    show (USnd v) = "snd " ++ show v
    show (UTopLevelEffect l p) = show l ++ show p

instance Show UComp where
    show (UVal v) = show v
    show (UApp f arg _) = inParens $ show f ++ " " ++ show arg
    show (USplit l x y v c) = undefined
    -- show (UCase v l x c y c') = "case " ++ show v ++ " { " ++ show l ++ " " ++ show x ++ "->" ++ show c ++ "; " ++ show y ++ " " ++ show c'
    show (UIf v c c') = "if " ++ show v ++ " then " ++ show c ++ " else " ++ show c'
    show (UAbsurd v) = "absurd " ++ show v
    show (ULet x varVal comp) = inParens $ "let " ++ x ++ " = " ++ show varVal ++ " in\n" ++ show comp


substV :: Var -> UValue -> UValue -> UValue
substV xS vS vT = case vT of
    UVar x -> if x == xS then vS else vT
    UNum _ -> vT
    UStr _ -> vT
    UBool _ -> vT
    ULambda x f ->
        if x == xS then ULambda x f
        else            ULambda x f'
        where f' = f >=> (return . subst xS vS)
    UUnit -> vT
    UPair vL vR -> UPair (_substV vL) (_substV vR)
    ULabel _ -> vT
    UEffLabel _ -> vT
    UBinOp op vL vR -> UBinOp op (_substV vL) (_substV vR)
    UFst v -> UFst (_substV v)
    USnd v -> USnd (_substV v)
    UTopLevelEffect l v -> UTopLevelEffect l (_substV v)
    where _substV = substV xS vS

subst :: Var -> UValue -> UComp -> UComp
subst xS vS cT = case cT of
    UVal v -> UVal (_substV v)
    UApp c1 v2 ks -> UApp (_subst c1) (_substV v2) ks
    UIf condV cT cF -> UIf (_substV condV) (_subst cT) (_subst cF)
    ULet x v c ->
        if x == xS then ULet x (_substV v) c
        else            ULet x (_substV v) (_subst c)
    UAbsurd v -> UAbsurd (_substV v)
    where _subst = subst xS vS
          _substV = substV xS vS
