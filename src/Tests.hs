{- HLINT ignore -}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Map as Map

import Freak
import Eval
import Types

genRow l n r = DPair (DLabel l) (DPair (DNum n) r)

testFromFile :: String -> Either Error DValue -> IO ()
testFromFile filename expected = do
    code <- readFile filename
    evalProgram code `shouldBe` expected

main :: IO ()
main = hspec $ do
  let row = "{a = 1; {b = 2; ()}}"
  let row' = "{c = 3; " ++ row ++ "}"
  let variant s = "["++s++" 1 ] : row"
  let dummyFunc = DLambda Map.empty (\x -> return (DNum 1))

  describe "Freak" $ do
    it "Basic arithmetic" $ do
      evalProgram "return 1" `shouldBe` (Right (DNum 1))
      evalProgram "return 2 + 3" `shouldBe` (Right (DNum 5))
      evalProgram "return 2 * 3" `shouldBe` (Right (DNum 6))
      evalProgram "return 5 - 2" `shouldBe` (Right (DNum 3))
      evalProgram "return 5 / 2" `shouldBe` (Right (DNum 2))
      evalProgram "return 6 / 2" `shouldBe` (Right (DNum 3))

    it "Order of operations" $ do
      evalProgram "return 2 + 3 * 2" `shouldBe` (Right (DNum 8))
      evalProgram "return 2 * 3 + 2" `shouldBe` (Right (DNum 8))
      evalProgram "return 2 * (3 + 2)" `shouldBe` (Right (DNum 10))

    it "Undefined variable" $ do
      evalProgram "return x" `shouldBe` (unboundVarErr "x")

    it "Absurd" $ do
      evalProgram "absurd 42" `shouldBe` absurdErr
      evalProgram "let x <- absurd 17 in return 42" `shouldBe` absurdErr

    it "Let expression" $ do
      evalProgram "let x <- return 3 in return x + 2" `shouldBe` (Right (DNum 5))
      evalProgram "let x <- return x in return x + 2" `shouldBe` (unboundVarErr "x")

    it "Nested let expression" $ do
      evalProgram "let x <- return 3 in let y <- return 2 in return x + y" `shouldBe` (Right (DNum 5))

    it "Variable hiding" $ do
      evalProgram "let x <- return 3 in let x <- return 2 in return x" `shouldBe` (Right (DNum 2))

    it "Return lambda" $ do
      evalProgram "return (\\x : int -> return x + 1)" `shouldBe` (Right dummyFunc)

    it "Static scope" $ do
      testFromFile "programs/staticScope.fk" (Right (DNum 3))

    it "Row" $ do
      evalProgram ("return "++row) `shouldBe` (Right (genRow "a" 1 (genRow "b" 2 DUnit)))

    it "Pair" $ do
      evalProgram ("return (1, 2)") `shouldBe` (Right (DPair (DNum 1) (DNum 2)))

    it "Nested pair" $ do
      evalProgram ("return ((1, 2), 3)") `shouldBe` (Right (DPair (DPair (DNum 1) (DNum 2)) (DNum 3)))

    it "Split" $ do
      evalProgram ("let (b = x; y) = "++row'++" in return 1 + x") `shouldBe` (Right (DNum 3))
      evalProgram ("let (a = x; y) = "++row'++" in return 1 + x") `shouldBe` (Right (DNum 2))

    it "Case" $ do
      let body = " {a x -> return x; y -> return 42}"
      evalProgram ("case "++variant "a"++body) `shouldBe` (Right (DNum 1))
      evalProgram ("case "++variant "c"++body) `shouldBe` (Right (DNum 42))

    it "Let lambda" $ do
      evalProgram "let f <- return (\\x : int -> return x + 1) in f 42" `shouldBe` (Right (DNum 43))

    it "Let lambda forget function result" $ do
      evalProgram "let x <- (\\x : int -> return x) 42 in return 1" `shouldBe` (Right (DNum 1))

    it "Let lambda forget function result - more verbose" $ do
      evalProgram "let f <- return (\\x : int -> return x + 1) in let x <- f 42 in return 1" `shouldBe` (Right (DNum 1))

    it "If statement" $ do
      evalProgram "if 1 then return 1 else return 0" `shouldBe` (Right (DNum 1))
      evalProgram "if 42 then return 1 else return 0" `shouldBe` (Right (DNum 1))
      evalProgram "if 0 then return 1 else return 0" `shouldBe` (Right (DNum 0))

    it "Lambda application" $ do
      evalProgram "(\\x : int -> return x + 1) 42" `shouldBe` (Right (DNum 43))

    it "Relational operators" $ do
      evalProgram "return 1 >= 1" `shouldBe` (Right (DNum 1))
      evalProgram "return 1 > 1" `shouldBe` (Right (DNum 0))
      evalProgram "return 42 >= 17" `shouldBe` (Right (DNum 1))
      evalProgram "return 42 <= 17" `shouldBe` (Right (DNum 0))
      evalProgram "return 2 == 2" `shouldBe` (Right (DNum 1))
      evalProgram "return 2 != 2" `shouldBe` (Right (DNum 0))
      evalProgram "return 2 < 3" `shouldBe` (Right (DNum 1))

    it "Branching" $ do
      evalProgram "if 2 >= 3 then return 1 else return 0" `shouldBe` (Right (DNum 0))

    -- Algebraic effects and handlers tests

    it "Trivial handler" $ do
      evalProgram "handle return 1 with {return x -> return x}" `shouldBe` (Right (DNum 1))

    it "Trivial handler with ops" $ do
      evalProgram "handle return 1 with {Op p r -> r 2 | return x -> return x }" `shouldBe` (Right (DNum 1))

    it "Constant effect handler" $ do
      evalProgram "handle do Const 1 with {Const p r -> r 42 | return x -> return x }" `shouldBe` (Right (DNum 42))

    it "Identity effect handler" $ do
      evalProgram "handle do Id 42 with {Id p r -> r p | return x -> return x }" `shouldBe` (Right (DNum 42))

    it "Compose let with do" $ do
      evalProgram "handle let x <- do Id 1 in return 3 with {Id p r -> r p | return x -> return x }" `shouldBe` (Right (DNum 3))

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

    it "Drop resumption result" $ do
      testFromFile "programs/dropResumption.fk" (Right (DNum 1))

    it "Sum of possible choices" $ do
      testFromFile "programs/choicesSum.fk" (Right (DNum 35))

    it "Max of possible choices" $ do
      testFromFile "programs/choicesMin.fk" (Right (DNum 5))

    it "List of possible choices" $ do
      let result = DPair (DPair (DNum 10) (DNum 5)) (DPair (DNum 20) (DNum 15))
      testFromFile "programs/choicesList.fk" (Right result)
