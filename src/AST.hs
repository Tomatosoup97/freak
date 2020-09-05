module AST where

import qualified Data.Map as Map
import Types

data VariantRow a = VariantRow RowType Label a
    deriving (Show, Eq)

type RecordRow a = Map.Map Label a -- todo: presence variables?

type AlgTheoryName = String
type Signature = [EffLabel]

inParens :: String -> String
inParens s = "(" ++ s ++ ")"

data Value
    = VVar Var
    | VNum Integer
    | VStr String
    | VLambda Var ValueType Comp
    | VFix Var Var Comp
    | VUnit
    | VPair Value Value
    | VRecordRow (RecordRow Value)
    | VExtendRow Label Value Value
    | VVariantRow (VariantRow Value)
    | VBinOp BinaryOp Value Value
    | VFst Value
    | VSnd Value
    deriving (Eq)

instance Show Value where
    show (VVar x) = x
    show (VNum n) = show n
    show (VStr s) = s
    show (VLambda x _ c) = inParens $ x ++ " -> " ++ show c
    show VUnit = "()"
    show (VPair vL vR) = inParens $ show vL ++ ", " ++ show vR
    show (VBinOp op vL vR) = inParens $ show vL ++ show op ++ show vR
    show (VFst v) = "fst " ++ show v
    show (VSnd v) = "snd " ++ show v

data BinaryOp
    = BAdd
    | BMul
    | BDiv
    | BSub
    | BLte
    | BLt
    | BGte
    | BGt
    | BEq
    | BNe
    deriving (Eq)

instance Show BinaryOp where
    show BAdd = "+"
    show BMul = "*"
    show BDiv = "/"
    show BSub = "-"
    show BLte = "<="
    show BLt = "<"
    show BGte = ">="
    show BGt = ">"
    show BEq = "=="
    show BNe = "!="

data Comp
    = EVal Value
    | ELet Var Comp Comp
    | EApp Value Value
    | ESplit Label Var Var Value Comp
    | ECase Value Label Var Comp Var Comp
    | EReturn Value
    | EAbsurd Value
    | EIf Value Comp Comp
    -- Algebraic effects
    | EOp EffLabel Value
    | EHandle Comp Handler
    -- Coalgebraic effects
    | ECoop EffLabel Value
    | ECohandle Comp Handler
    -- Intermediate representation for a cohandler
    | ECohandleIR AlgTClause Comp Handler
    deriving (Eq)

instance Show Comp where
    show (EVal v) = show v
    show (ELet x xComp comp) = inParens $ "let " ++ x ++ " = " ++ show xComp ++ " in\n" ++ show comp
    show (EApp f arg) = inParens $ show f ++ " " ++ show arg
    show (EReturn v) = "return " ++ show v
    show (EAbsurd v) = "absurd " ++ show v
    show (EIf v c c') = "if " ++ show v ++ " then " ++ show c ++ " else " ++ show c'
    show (EOp l v) = "do " ++ show l ++ " " ++ show v
    show (EHandle c h) = "handle\n" ++ show c ++ "\nwith {\n" ++ show h ++ "\n}"
    show (ECoop l v) = "observe " ++ show l ++ " " ++ show v
    show (ECohandle c h) = "cohandle " ++ show c ++ "through { " ++ show h ++ " }"
    show (ECohandleIR algT c h) = "cohandle " ++ show algT ++ " at\n" ++ show c ++ "\nthrough { \n" ++ show h ++ "\n}"

data AlgTClause = AlgTC AlgTheoryName Value
    deriving (Eq)

instance Show AlgTClause where
    show (AlgTC algT initV) = algT ++ " using " ++ show initV

data Handler
    = HRet Var Comp
    | HOps AlgebraicOp Handler
    deriving (Eq)

instance Show Handler where
    show (HOps op h) = show op ++ " |\n" ++ show h
    show (HRet x c) = "return " ++ x ++ " -> " ++ show c

data AlgebraicOp = AlgOp EffLabel Var Var Comp
    deriving (Eq)

instance Show AlgebraicOp where
    show (AlgOp l p r c) = show l ++ " " ++ p ++ " " ++ r ++ " -> " ++ show c

-- todo: generalize these with Functor and catamorphism

hret :: Handler -> Handler
hret h@(HRet _ _) = h
hret (HOps _ h) = hret h

-- todo: these should be rows
hops :: Handler -> [AlgebraicOp] -- todo: make it a set
hops = aux []
    where aux acc (HRet _ _) = acc
          aux acc (HOps op h) = aux (op:acc) h

opL :: AlgebraicOp -> EffLabel
opL (AlgOp l _ _ _) = l

hopsL :: Handler -> Signature
hopsL h = map opL (hops h)

hop :: EffLabel -> Handler -> Maybe AlgebraicOp
hop _ (HRet _ _) = Nothing
hop l (HOps op@(AlgOp l' _ _ _) h)
    | l == l' = return op
    | otherwise = hop l h

leftJoinHandlers :: Handler -> Handler -> Handler
leftJoinHandlers hL = aux
    where aux (HOps op h) = HOps op (aux h)
          aux (HRet _ _) = hL
