module Desugar where

import Control.Monad
import Types
import AST
import DesugarCoalg
import MonadicTraverse

desugarFinally :: Comp -> Either Error Comp
desugarFinally = astTraverser (AstF return gC return)
    where gC (ECohandleIRFinally algT bC (FinallyC x fC) h) =
            return $ ECohandleIR algT (ELet x bC fC) h
          gC c = return c

desugar :: Comp -> Either Error Comp
desugar = desugarFinally >=> return . desugarCoalg
