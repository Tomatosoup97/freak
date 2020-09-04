module AST where

import qualified Data.Map as Map
import Types
import Control.Monad.State.Lazy
import Control.Monad.Except

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
    | ECohandleIR AlgTheoryName Value Comp Handler
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
    show (ECohandleIR algT initV c h) = "cohandle " ++ algT ++ " using " ++ show initV ++ " at\n" ++ show c ++ "\nthrough { \n" ++ show h ++ "\n}"

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

freshAlphaVar :: Var -> ExceptT Error (State Int) Var
freshAlphaVar s = do
    n <- lift get
    lift $ put (n+1)
    return $ toAlphaVar n s

toAlphaVar :: Int -> Var -> Var
toAlphaVar n s = "!alpha" ++ show n ++ "_" ++ s

type VarMap = Map.Map Var Var

_alphaConvV :: VarMap -> Value -> ExceptT Error (State Int) Value
_alphaConvV vmap v = case v of
    VVar x -> case Map.lookup x vmap of
        Just x' -> return $ VVar x'
        Nothing -> throwError $ unboundVarErr x
    VNum n -> return v
    VStr s -> return v
    VLambda x vType c -> do
        x' <- freshAlphaVar x
        let vmap' = Map.insert x x' vmap
        VLambda x' vType <$> _alphaConv vmap' c
    VUnit -> return v
    VPair vL vR -> do
        vL' <- alphaV vL
        VPair vL' <$> alphaV vR
    VBinOp op vL vR -> do
        vL' <- alphaV vL
        VBinOp op vL' <$> alphaV vR
    VFst v -> VFst <$> alphaV v
    VSnd v -> VSnd <$> alphaV v
    where alphaV = _alphaConvV vmap

_alphaConvH :: VarMap -> Handler -> ExceptT Error (State Int) Handler
_alphaConvH vmap (HRet x c) = do
    x' <- freshAlphaVar x
    let vmap' = Map.insert x x' vmap
    HRet x' <$> _alphaConv vmap' c

_alphaConvH vmap (HOps (AlgOp l p r c) h) = do
    r' <- freshAlphaVar r
    p' <- freshAlphaVar p
    let vmap' = Map.insert r r' (Map.insert p p' vmap)
    c' <- _alphaConv vmap' c
    HOps (AlgOp l p' r' c') <$> _alphaConvH vmap h

_alphaConv :: VarMap -> Comp -> ExceptT Error (State Int) Comp
_alphaConv vmap c = case c of
    EVal v -> EVal <$> alphaV v
    ELet x vC bC -> do
        x' <- freshAlphaVar x
        let vmap' = Map.insert x x' vmap
        vC' <- _alphaConv vmap vC
        bC' <- _alphaConv vmap' bC
        return $ ELet x' vC' bC'
    EApp f arg -> do
        f' <- alphaV f
        arg' <- alphaV arg
        return $ EApp f' arg'
    EReturn v -> EReturn <$> alphaV v
    EAbsurd v -> EAbsurd <$> alphaV v
    EIf condV cT cF -> do
        cT' <- alpha cT
        cF' <- alpha cF
        convV' <- alphaV condV
        return $ EIf convV' cT' cF'
    EOp l v -> EOp l <$> alphaV v
    EHandle c h -> do
        c' <- alpha c
        EHandle c' <$> alphaH h
    ECoop l v -> ECoop l <$> alphaV v
    ECohandle c h -> do
        c' <- alpha c
        ECohandle c' <$> alphaH h
    ECohandleIR algT v c h -> do
        v' <- alphaV v
        c' <- alpha c
        ECohandleIR algT v' c' <$> alphaH h
    where alpha = _alphaConv vmap
          alphaV = _alphaConvV vmap
          alphaH = _alphaConvH vmap

alphaConvert :: Comp -> Either Error Comp
alphaConvert c = evalState (runExceptT (_alphaConv Map.empty c)) 0
