module AST where

type Var = String

data Type
    = TInt
    | TBool
    deriving (Eq)

data Value
    = VVar Var
    | VNum Integer
    | VBool Bool
    deriving (Eq)

data BinaryOp
    = BAdd
    | BMul
    deriving (Eq)

data Expr
    = EBinOp BinaryOp Expr Expr
    | EVal Value
    deriving (Eq)

data Comp
    = CLet Var Comp Comp
    | CRet Expr
    deriving (Eq)

data Term
    = TExpr Expr
    | TComp Comp
    deriving (Eq)

instance Show Type where
    showsPrec _ TInt = showString "int"
    showsPrec _ TBool = showString "bool"

instance Show Value where
    showsPrec _ (VVar x) = showString x
    showsPrec _ (VNum n) = shows n
    showsPrec _ (VBool b) = shows b

instance Show BinaryOp where
    show BAdd = "+"
    show BMul = "*"

instance Show Expr where
    showsPrec p (EBinOp op e1 e2) =
        showParen (p > p')
          (showsPrec leftPrec e1 . showChar ' ' . shows op . showChar ' '
          . showsPrec rightPrec e2)
        where
          (p',leftPrec,rightPrec) = binOpPrec op
          binOpPrec :: BinaryOp -> (Int, Int, Int)
          binOpPrec BAdd = (50, 50, 51)
          binOpPrec BMul = (60, 60, 61)

    showsPrec _ (EVal val) = shows val

instance Show Comp where
    showsPrec p (CLet x c1 c2) =
        showParen (p > 0)
        (showString "let " . showString x . showString " <- " . showsPrec 0 c1
        . showString " in " . showsPrec 0 c2)

    showsPrec _ (CRet e) = showString "return " . shows e

instance Show Term where
    showsPrec _ (TExpr e) = shows e
    showsPrec _ (TComp c) = shows c
