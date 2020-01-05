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
runProgram s =
    case evalProgram s of
        Left e -> print $ show e
        Right v -> print $ show v


runFile :: String -> IO ()
runFile file = readFile file >>= runProgram
