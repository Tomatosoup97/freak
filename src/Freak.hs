{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module Freak where

import Control.Monad.Except
import Parser
import AST
import AlphaConv
import TargetAST
import CPS
import DValue
import Eval
import Types
import Transform


cpsProgram :: String -> EvalResMonad UComp
cpsProgram s = case parseString s of
    Right ast -> case alphaConvert ast of
        Right ast' -> runCPS $ transform ast'
        Left err -> return $ Left err
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
runFromFile = readFile >=> runProgram

cpsFromFile :: String -> IO ()
cpsFromFile f = readFile f >>= cpsProgram >>= \case
    Right cps -> print cps
    Left err -> print err >> fail "CPS translation error"

parseFile :: String -> IO ()
parseFile = parseFromFile >=> print

parseDesugarFile :: String -> IO ()
parseDesugarFile = parseFromFile >=> (print . transform)

validateFile :: String -> IO ()
validateFile = readFile >=> cpsProgram >=> (\_ -> return ())
