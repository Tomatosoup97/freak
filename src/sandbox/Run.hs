import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Eval
import AST
import CPS
import Types

outputRes :: Expr -> IO ()
outputRes e' = do
    let e = runCPS e'
    print $ e
    case e of
      Right e -> print $ eval Map.empty e
      Left m -> print m

main :: IO ()
main = do
    let int = EVal . VNum
    let plus = EBinOp BAdd (int 2) (EVal (VVar "x"))
    let lambda = EVal (VLambda "x" TInt plus)
    let app = EApp lambda (int 3)
    let letTerm = ELet "x" (int 4) plus
    outputRes app
    outputRes letTerm
