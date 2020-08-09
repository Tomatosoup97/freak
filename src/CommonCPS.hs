module CommonCPS where

import Control.Monad.Except
import Control.Monad.Trans.State.Lazy
import Types

type CPSMonad a = ExceptT Error (StateT Int IO) a

freshVar' :: String -> CPSMonad Var
freshVar' s = do
    n <- lift get
    lift $ put (n+1)
    return $ s ++ show n

freshVar :: CPSMonad Var
freshVar = freshVar' "__m" -- todo: this should be unique!
