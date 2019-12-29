import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.State
import Eval
import AST
import CPS
import Types

outputRes :: Show a => a -> Expr -> IO ()
outputRes expected e' = do
    let msg = "Expected: " ++ show expected ++ ", actual: "
    let e = runCPS e'
    -- print $ e
    case e of
      Right e -> print $ msg ++ show (eval Map.empty e)
      Left m -> print m

main :: IO ()
main = do
    let int = EVal . VNum
    let plus = EBinOp BAdd (int 2) (EVal (VVar "x"))
    let lambda = EVal (VLambda "x" TInt plus)
    let app = EApp lambda (int 3)
    let letTerm = ELet "x" (int 4) plus
    let row = VRecordRow (RecordRowExtend "r" (VNum 1) (RecordRowExtend "l" (VNum 2) RecordRowUnit))
    let split = ESplit "l" "x" "y" row plus
    let variant = VVariantRow $ VariantRow RowType "l" (VNum 2)
    let variant' = VVariantRow $ VariantRow RowType "l'" (VNum 3)
    let getCase row = ECase row "l" "x" plus "y" (int 1)

    outputRes 5 app
    outputRes 6 letTerm
    outputRes 4 split
    outputRes 4 (getCase variant)
    outputRes 1 (getCase variant')
