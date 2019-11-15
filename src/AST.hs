module AST where

import Types

type Var = String

data Value
    = VVar Var
    | VNum Integer
    | VBool Bool
    | VLambda Var ValueType Comp
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
    | CApp Value Value
    deriving (Eq)

data Term
    = TermExpr Expr
    | TermComp Comp
    deriving (Eq)


addParens :: ShowS -> ShowS
addParens s = showString "(" . s . showString ")"

instance Show Value where
    showsPrec _ (VVar x) = showString x
    showsPrec _ (VNum n) = shows n
    showsPrec _ (VBool b) = shows b
    showsPrec _ (VLambda var t c) =
        showString "λ" . showString var . showString " -> " . shows c

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
    showsPrec _ (CApp v1 v2) = addParens (shows v1) . showString " " . shows v2

instance Show Term where
    showsPrec _ (TermExpr e) = shows e
    showsPrec _ (TermComp c) = shows c
