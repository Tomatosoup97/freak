module AST where

import qualified Data.Map as Map
import Types

type Var = String
type Label = String

data VariantRow a = VariantRow RowType Label a
    deriving (Show, Eq)

type RecordRow a = Map.Map Label a -- todo: presence variables?

data Value
    = VVar Var
    | VNum Integer
    | VLambda Var ValueType Comp
    | VFix Var Var Comp
    | VUnit
    | VRecordRow (RecordRow Value)
    | VExtendRow Label Value Value
    | VVariantRow (VariantRow Value)
    | VBinOp BinaryOp Value Value
    deriving (Show, Eq)

data BinaryOp
    = BAdd
    | BMul
    deriving (Eq)

instance Show BinaryOp where
    show BAdd = "+"
    show BMul = "*"

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
    | EDo Label Value
    | EHandle Comp Handler
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

hop :: Label -> Handler -> Maybe AlgebraicOp
hop _ (HRet _ _) = Nothing
hop l (HOps op@(AlgOp l' _ _ _) h)
    | l == l' = return op
    | otherwise = hop l h
