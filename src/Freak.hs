{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Freak where

import Control.Monad.Except
import Parser
import AST
import TargetAST
import CPS
import DValue
import Eval
import Types
import Transform


cpsProgram :: String -> EvalResMonad UComp
cpsProgram s = case parseString s of
    Right ast -> runCPS $ transform ast
    Left err -> return $ Left err

evalProgram :: String -> EvalResMonad DValue
evalProgram s = cpsProgram s >>= \case
    Left e -> return $ Left e
    Right c -> runEval c

runProgram :: String -> IO ()
runProgram s = evalProgram s >>= \case
    Left e -> print e
    Right v -> print v

runFromFile :: String -> IO ()
runFromFile filename = readFile filename >>= runProgram

cpsFromFile :: String -> IO ()
cpsFromFile filename = readFile filename >>= cpsProgram >>= \case
    Right cps -> print cps
    Left err -> print err >> fail "CPS translation error"

parseFile :: String -> Bool -> IO ()
parseFile filename quiet = do
    comp <- parseFromFile filename
    if quiet then return () else print comp
