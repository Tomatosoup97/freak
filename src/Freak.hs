{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Freak where

import Control.Monad.Except
import Parser
import AST
import TargetAST
import CPS
import CommonEval
import Eval
import Types

cpsProgram :: String -> EvalResMonad UComp
cpsProgram s = case parseString s of
    Right ast -> runCPS ast
    Left err -> return $ Left err

evalProgram :: String -> EvalResMonad DValue
evalProgram s = cpsProgram s >>= \case
    Left e -> return $ Left e
    Right c -> runEval c

runProgram :: String -> IO ()
runProgram s = evalProgram s >>= \case
    Left e -> print $ show e
    Right v -> print $ show v

runFromFile :: String -> IO ()
runFromFile filename = readFile filename >>= runProgram

outputResult :: Either Error DValue -> IO ()
outputResult (Left e) = print $ show e
outputResult (Right v) = print $ show v
