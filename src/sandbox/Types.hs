module Types where

data ValueType
    = TInt
    | TBool
    | TLambda ValueType CompType
    deriving (Eq)

data EffectType
    = RowType -- TODO
    deriving (Eq)

data CompType
    = TComp ValueType EffectType
    deriving (Eq)

data Type
    = TVal ValueType
    | TC CompType
    deriving (Eq)

instance Show ValueType where
    showsPrec _ TInt = showString "int"
    showsPrec _ TBool = showString "bool"
    showsPrec _ (TLambda varT compT) = shows varT . showString " -> " . shows compT

instance Show Type where
    showsPrec _ (TVal tv) = shows tv
    showsPrec _ (TC tc) = shows tc

instance Show CompType where
    showsPrec _ (TComp tv te) = shows tv . showString "!" . shows te

instance Show EffectType where
    showsPrec _ (RowType) = showString "{ row }" -- TODO
