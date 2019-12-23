module TargetAST where

type UVar = String
type Label = String

data UValue
    = UVar UVar
    | UNum Integer
    | UBool Bool
    | ULambda UVar UComp
    | UUnit
    | UPair UValue UValue
    | ULabel Label
    | URec UVar UVar UComp
    deriving (Show, Eq)

data UBinaryOp
    = UAdd
    | UMul
    deriving (Show, Eq)

data UExpr
    = UBinOp UBinaryOp UExpr UExpr
    | UVal UValue
    deriving (Show, Eq)

data UComp
    = USplit UVar UVar UValue UComp
    | UApp UComp UComp
    | UExpr UExpr
    | UCase UValue Label UComp UVar UComp
    | UAbsurd UExpr
    deriving (Show, Eq)
