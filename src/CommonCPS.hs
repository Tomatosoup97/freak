module CommonCPS where

import Control.Monad.Except
import Control.Monad.State
import Types

type CPSMonad a = ExceptT Error (State Int) a

freshVar' :: String -> CPSMonad Var
freshVar' s = do
    n <- get
    put (n+1)
    return $ s ++ show n

freshVar :: CPSMonad Var
freshVar = freshVar' "__m" -- todo: this should be unique!
