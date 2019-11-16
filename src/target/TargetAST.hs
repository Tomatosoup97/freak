module TargetAST where

type UVar = String

data UValue
    = UVar UVar
    | UNum Integer
    | UBool Bool
    | ULambda UVar UComp
    -- TODO: rec
    | UUnit
    | UPairr UValue UValue
    -- TODO: labels
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
    | UApp UComp UValue
    | UExpr UExpr
    -- TODO: case
    -- TODO: absurd
    deriving (Show, Eq)
