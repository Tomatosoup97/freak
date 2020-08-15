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
    code <- readFile filename
    case parseString code of
        Right r -> if quiet then return () else print r
        Left e  -> print e >> fail "Parse error"
