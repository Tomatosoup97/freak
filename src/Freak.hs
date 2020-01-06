module Freak where

import Parser
import AST
import CPS
import Eval
import Types


evalProgram :: String -> Either Error DValue
evalProgram s = do
    ast <- parseString s
    cpsTerm <- runCPS ast
    runEval cpsTerm

runProgram :: String -> IO ()
runProgram = outputResult . evalProgram

runFromFile :: String -> IO ()
runFromFile filename = readFile filename >>= runProgram

outputResult :: Either Error DValue -> IO ()
outputResult (Left e) = print $ show e
outputResult (Right v) = print $ show v
