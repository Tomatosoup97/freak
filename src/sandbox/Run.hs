import Eval
import AST
import CPS
import Types
import qualified Data.Map as Map

main = do
    let int = EVal . VNum
    let plus = EBinOp BAdd (int 2) (EVal (VVar "y"))
    let lambda = EVal (VLambda "y" TInt plus)
    let app = EApp lambda (int 3)
    print $ app
    print $ cps app initialCont
    print $ eval Map.empty (cps app initialCont)

