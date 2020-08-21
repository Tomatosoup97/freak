module AST where

import qualified Data.Map as Map
import Types

data VariantRow a = VariantRow RowType Label a
    deriving (Show, Eq)

type RecordRow a = Map.Map Label a -- todo: presence variables?

type AlgTheoryName = String
type Signature = [Label]

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
    deriving (Show, Eq)

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
    | EOp Label Value
    | EHandle Comp Handler
    -- Coalgebraic effects
    | ECoop Label Value
    | ECohandle Comp Handler
    -- Intermediate representation for a cohandler
    | ECohandleIR AlgTheoryName Value Comp Handler
    deriving (Show, Eq)

data Handler
    = HRet Var Comp
    | HOps AlgebraicOp Handler
    deriving (Show, Eq)

data AlgebraicOp = AlgOp Label Var Var Comp
    deriving (Show, Eq)

-- todo: generalize these with Functor and catamorphism

hret :: Handler -> Handler
hret h@(HRet _ _) = h
hret (HOps _ h) = hret h

-- todo: these should be rows
hops :: Handler -> [AlgebraicOp] -- todo: make it a set
hops = aux []
    where aux acc (HRet _ _) = acc
          aux acc (HOps op h) = aux (op:acc) h

opL :: AlgebraicOp -> Label
opL (AlgOp l _ _ _) = l

hopsL :: Handler -> Signature
hopsL h = map opL (hops h)

hop :: Label -> Handler -> Maybe AlgebraicOp
hop _ (HRet _ _) = Nothing
hop l (HOps op@(AlgOp l' _ _ _) h)
    | l == l' = return op
    | otherwise = hop l h
