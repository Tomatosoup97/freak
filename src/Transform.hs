module Transform where

import AST
import DesugarFinally
import DesugarCoalg

transform :: Comp -> Comp
transform = desugarCoalg . desugarFinally
