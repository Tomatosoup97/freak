{- HLINT ignore -}
{-# LANGUAGE LambdaCase #-}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Map as Map

import Control.Monad.Trans.Class
import Freak
import Eval
import CommonEval
import Types
import TargetAST

genRow l n r = DPair (DLabel l) (DPair (DNum n) r)

shouldBeT m b = do
    r <- m
    r `shouldBe` b

testFromFile :: String -> Either Error DValue -> IO ()
testFromFile filename expected = do
    code <- readFile filename
    evalProgram code `shouldBeT` expected

main :: IO ()
main = hspec $ do
  let row = "{a = 1; {b = 2; ()}}"
  let row' = "{c = 3; " ++ row ++ "}"
  let variant s = "["++s++" 1 ] : row"

  describe "Freak" $ do
    it "Basic arithmetic" $ do
      evalProgram "return 1" `shouldBeT` (Right (DNum 1))
      evalProgram "return 2 + 3" `shouldBeT` (Right (DNum 5))
      evalProgram "return 2 * 3" `shouldBeT` (Right (DNum 6))
      evalProgram "return 5 - 2" `shouldBeT` (Right (DNum 3))
      evalProgram "return 5 / 2" `shouldBeT` (Right (DNum 2))
      evalProgram "return 6 / 2" `shouldBeT` (Right (DNum 3))

    it "Strings" $ do
      evalProgram "return \"value\"" `shouldBeT` (Right (DStr "value"))
      evalProgram "return \"x\"" `shouldBeT` (Right (DStr "x"))
      evalProgram "return \"this is string\"" `shouldBeT` (Right (DStr "this is string"))
      evalProgram "return \"x1\"" `shouldBeT` (Right (DStr "x1"))
      evalProgram "let x <- return \"y\" in return x" `shouldBeT` (Right (DStr "y"))

    it "Order of operations" $ do
      evalProgram "return 2 + 3 * 2" `shouldBeT` (Right (DNum 8))
      evalProgram "return 2 * 3 + 2" `shouldBeT` (Right (DNum 8))
      evalProgram "return 2 * (3 + 2)" `shouldBeT` (Right (DNum 10))

    it "Undefined variable" $ do
      evalProgram "return x" `shouldBeT` (Left (unboundVarErr "x"))

    it "Absurd" $ do
      evalProgram "absurd 42" `shouldBeT` (Left (absurdErr (UNum 42)))
      evalProgram "let x <- absurd 17 in return 42" `shouldBeT` (Left (absurdErr (UNum 17)))

    it "Let expression" $ do
      evalProgram "let x <- return 3 in return x + 2" `shouldBeT` (Right (DNum 5))

    it "Let expression undefined var" $ do
      evalProgram "let x <- return x in return x + 2" `shouldBeT` (Left (unboundVarErr "x"))

    it "Nested let expression" $ do
      evalProgram "let x <- return 3 in let y <- return 2 in return x + y" `shouldBeT` (Right (DNum 5))

    it "Variable hiding" $ do
      evalProgram "let x <- return 3 in let x <- return 2 in return x" `shouldBeT` (Right (DNum 2))

    it "Static scope" $ do
      testFromFile "programs/staticScope.fk" (Right (DNum 3))

    -- it "Row" $ do
    --   evalProgram ("return "++row) `shouldBeT` (Right (genRow "a" 1 (genRow "b" 2 DUnit)))

    it "Pair" $ do
      evalProgram ("return (1, 2)") `shouldBeT` (Right (DPair (DNum 1) (DNum 2)))

    it "Nested pair" $ do
      evalProgram ("return ((1, 2), 3)") `shouldBeT` (Right (DPair (DPair (DNum 1) (DNum 2)) (DNum 3)))

    -- it "Split" $ do
    --   evalProgram ("let (b = x; y) = "++row'++" in return 1 + x") `shouldBeT` (Right (DNum 3))
    --   evalProgram ("let (a = x; y) = "++row'++" in return 1 + x") `shouldBeT` (Right (DNum 2))

    -- it "Case" $ do
    --   let body = " {a x -> return x; y -> return 42}"
    --   evalProgram ("case "++variant "a"++body) `shouldBeT` (Right (DNum 1))
    --   evalProgram ("case "++variant "c"++body) `shouldBeT` (Right (DNum 42))

    it "Let lambda" $ do
      evalProgram "let f <- return (\\x : int -> return x + 1) in f 42" `shouldBeT` (Right (DNum 43))

    it "Application" $ do
      evalProgram "(\\x : int -> return x + 1) 1" `shouldBeT` (Right (DNum 2))

    it "Let application" $ do
      evalProgram "let x <- (\\x : int -> return x + 1) 1 in return 3" `shouldBeT` (Right (DNum 3))
      evalProgram "let x <- (\\x : int -> return x + 1) 1 in return x + 1" `shouldBeT` (Right (DNum 3))

    it "Let lambda forget function result" $ do
      evalProgram "let x <- (\\x : int -> return x) 42 in return 1" `shouldBeT` (Right (DNum 1))

    it "Let lambda forget function result - more verbose" $ do
      testFromFile "programs/letLambda.fk" (Right (DNum 1))

    it "If statement" $ do
      evalProgram "if 1 then return 1 else return 0" `shouldBeT` (Right (DNum 1))
      evalProgram "if 42 then return 1 else return 0" `shouldBeT` (Right (DNum 1))
      evalProgram "if 0 then return 1 else return 0" `shouldBeT` (Right (DNum 0))

    it "Lambda application" $ do
      evalProgram "(\\x : int -> return x + 1) 42" `shouldBeT` (Right (DNum 43))

    it "Relational operators" $ do
      evalProgram "return 1 >= 1" `shouldBeT` (Right (DNum 1))
      evalProgram "return 1 > 1" `shouldBeT` (Right (DNum 0))
      evalProgram "return 42 >= 17" `shouldBeT` (Right (DNum 1))
      evalProgram "return 42 <= 17" `shouldBeT` (Right (DNum 0))
      evalProgram "return 2 == 2" `shouldBeT` (Right (DNum 1))
      evalProgram "return 2 != 2" `shouldBeT` (Right (DNum 0))
      evalProgram "return 2 < 3" `shouldBeT` (Right (DNum 1))

    it "Branching" $ do
      evalProgram "if 2 >= 3 then return 1 else return 0" `shouldBeT` (Right (DNum 0))

    -- Algebraic effects and handlers tests

    it "Trivial handler" $ do
      evalProgram "handle return 1 with {return x -> return x}" `shouldBeT` (Right (DNum 1))

    it "Trivial handler with ops" $ do
      evalProgram "handle return 1 with {Op p r -> r 2 | return x -> return x }" `shouldBeT` (Right (DNum 1))

    it "Constant effect handler" $ do
      evalProgram "handle do Const 1 with {Const p r -> r 42 | return x -> return x }" `shouldBeT` (Right (DNum 42))

    it "Identity effect handler" $ do
      evalProgram "handle do Id 42 with {Id p r -> r p | return x -> return x }" `shouldBeT` (Right (DNum 42))

    it "Compose let with do" $ do
      evalProgram "handle let x <- do Id 1 in return 3 with {Id p r -> r p | return x -> return x }" `shouldBeT` (Right (DNum 3))

    it "Increment effect handler" $ do
      testFromFile "programs/increment.fk" (Right (DNum 18))

    it "Simulate exceptions" $ do
      testFromFile "programs/exceptions.fk" (Right (DNum 42))

    it "Double increment effect handler" $ do
      testFromFile "programs/doubleIncrement.fk" (Right (DNum 3))

    it "Compose effects" $ do
      testFromFile "programs/composeEffects.fk" (Right (DNum 7))

    it "Deep handlers" $ do
      testFromFile "programs/deepHandlers.fk" (Right (DNum 7))

    it "Unhandled effect" $ do
      testFromFile "programs/unhandledEffect.fk" (Left (absurdErr (DPair (DLabel "Effect") DUnit)))
      evalProgram "handle do Nothing 42 with {return x -> return x}" `shouldBeT` (Left (absurdErr (DPair (DLabel "Nothing") (DNum 42))))
      evalProgram "do Nothing 42" `shouldBeT` (Left (absurdErr (DPair (DLabel "Nothing") (DNum 42))))

    it "Print effect" $ do
      evalProgram "do Print \"print\"" `shouldBeT` (Right DUnit)
      evalProgram "let x <- do Print \"print\" in return 1" `shouldBeT` (Right (DNum 1))
      testFromFile "programs/io/printToConsole.fk" (Right DUnit)

    -- it "Drop resumption result" $ do
    --   testFromFile "programs/dropResumption.fk" (Right (DNum 1))

    -- it "Sum of possible choices" $ do
    --   testFromFile "programs/choicesSum.fk" (Right (DNum 35))

    -- it "Min of possible choices" $ do
    --   testFromFile "programs/choicesMin.fk" (Right (DNum 5))

    -- it "List of possible choices" $ do
    --   let result = DPair (DPair (DNum 10) (DNum 5)) (DPair (DNum 20) (DNum 15))
    --   testFromFile "programs/choicesList.fk" (Right result)

    it "Let handler" $ do
      testFromFile "programs/letHandler.fk" (Right (DNum 14))

    -- TODO: Support named, reusable handlers
    -- it "Lambda handler" $ do
    --  testFromFile "programs/lambdaHandler.fk" (Right (DNum 42))

    it "Nested handlers" $ do
      testFromFile "programs/nestedHandlers.fk" (Right (DNum 4))

    it "Complex nested handlers" $ do
      testFromFile "programs/complexNestedHandlers.fk" (Right (DNum 15))

    it "File handling" $ do
      testFromFile "programs/io/writeToFile.fk" (Right (DStr "one-two-three"))

    -- Coalgebraic effects

    it "[Coalg] Increment coeffect handler" $ do
      testFromFile "programs/coeffects/increment.fk" (Right (DNum 18))

    it "[Coalg] Nested cohandlers" $ do
      testFromFile "programs/coeffects/nestedHandlers.fk" (Right (DNum 4))

    it "[Coalg] Deep cohandlers" $ do
      testFromFile "programs/coeffects/deepHandlers.fk" (Right (DNum 7))

    -- Both effects

    it "[Both] Mixing algebras should result in error" $ do
      testFromFile "programs/bothEffects/fileOpsMix.fk" (Left (CPSError "effect cannot enter world of coeffects!"))
      testFromFile "programs/bothEffects/effectsMix.fk" (Left (CPSError "coeffect cannot enter world of effects!"))

    it "[Both] Subset interleave of effects" $ do
      testFromFile "programs/bothEffects/fileOpsCorrect.fk" (Right DUnit)

    it "[Both] Disjoint interleave of effects" $ do
      testFromFile "programs/bothEffects/basicCorrectUseCase.fk" (Right (DNum 15))
