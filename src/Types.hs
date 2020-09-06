module Types where

import Control.Monad.Except
import Control.Monad.Trans.State.Lazy

-- TODO: Reconsider name
type EvalMonad a = ExceptT Error (StateT Int IO) a

type EvalResMonad a = IO (Either Error a)

type Var = String
type Label = String

data EffLabel = EffL String
              | CoeffL String
              deriving (Eq, Ord, Show)

data Error
    = EvalError String
    | CPSError String
    | ParseError String
    | StaticAnalyzerError String
    deriving (Eq, Show)

unboundVarErr :: String -> Error
unboundVarErr x = EvalError $ "Unbound variable " ++ x

absurdErr :: Show a => a -> Error
absurdErr x = EvalError $ "Absurd; divergent term: " ++ show x

invalidUseOfEffErr :: Error
invalidUseOfEffErr = StaticAnalyzerError "\
    \You can't use effects in cohandler or finally clause!"

linearContUsageErr :: Error
linearContUsageErr = StaticAnalyzerError "\
    \Resumptions in cohandlers should be used linearly!"

data ValueType
    = TInt
    | TBool
    | TLambda ValueType CompType
    deriving (Eq)

data RowType = RowType -- TODO
    deriving (Eq)

newtype EffectType = ERowType RowType
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

instance Show RowType where
    showsPrec _ RowType = showString "Row " -- TODO

instance Show EffectType where
    showsPrec _ (ERowType row) = showString $ "{ " ++ show row ++ " }"
