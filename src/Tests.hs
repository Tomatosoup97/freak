{- HLINT ignore -}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Data.Map as Map

import Freak
import Eval
import Types

genRow l n r = DPair (DLabel l) (DPair (DNum n) r)

main :: IO ()
main = hspec $ do
  let row = "(a = 1; (b = 2; ()))"
  let row' = "(c = 3; " ++ row ++ ")"
  let variant s = "["++s++" 1 ] : row"
  let dummyFunc = DLambda Map.empty (\x -> return (DNum 1))

  describe "Freak" $ do
    it "Basic arithmetic" $ do
      evalProgram "return 1" `shouldBe` (Right (DNum 1))
      evalProgram "return 2 + 3" `shouldBe` (Right (DNum 5))
      evalProgram "return 2 * 3" `shouldBe` (Right (DNum 6))

    it "Order of operations" $ do
      evalProgram "return 2 + 3 * 2" `shouldBe` (Right (DNum 8))
      evalProgram "return 2 * 3 + 2" `shouldBe` (Right (DNum 8))
      evalProgram "return 2 * (3 + 2)" `shouldBe` (Right (DNum 10))
      -- evalProgram "return (2 + 3)" `shouldBe` (Right (DNum 5))
      -- evalProgram "return (2 + 3) * 2" `shouldBe` (Right (DNum 10))

    it "Undefined variable" $ do
      evalProgram "return x" `shouldBe` (unboundVarErr "x")

    it "Absurd" $ do
      evalProgram "absurd 42" `shouldBe` absurdErr
      evalProgram "let x <- absurd 17 in return 42" `shouldBe` absurdErr

    it "Let expression" $ do
      evalProgram "let x <- return 3 in return x + 2" `shouldBe` (Right (DNum 5))
      evalProgram "let x <- return x in return x + 2" `shouldBe` (unboundVarErr "x")

    it "Return lambda" $ do
      evalProgram "return (\\x : int -> return x + 1)" `shouldBe` (Right dummyFunc)

    it "Row" $ do
      evalProgram ("return "++row) `shouldBe` (Right (genRow "a" 1 (genRow "b" 2 DUnit)))

    it "Split" $ do
      evalProgram ("let (b = x; y) = "++row'++" in return 1 + x") `shouldBe` (Right (DNum 3))
      evalProgram ("let (a = x; y) = "++row'++" in return 1 + x") `shouldBe` (Right (DNum 2))

    it "Case" $ do
      let body = " {a x -> return x; y -> return 42}"
      evalProgram ("case "++variant "a"++body) `shouldBe` (Right (DNum 1))
      evalProgram ("case "++variant "c"++body) `shouldBe` (Right (DNum 42))

    it "Let lambda" $ do
      evalProgram "let f <- return (\\x : int -> return x + 1) in f 42" `shouldBe` (Right (DNum 43))

    it "Lambda application" $ do
      evalProgram "(\\x : int -> return x + 1) 42" `shouldBe` (Right (DNum 43))

    it "Trivial handler" $ do
      evalProgram "handle return 1 with {return x -> return x}" `shouldBe` (Right (DNum 1))

    it "Trivial handler with ops" $ do
      evalProgram "handle return 1 with {Op p r -> r 2 | return x -> return x }" `shouldBe` (Right (DNum 1))

    it "Constant effect handler" $ do
      evalProgram "handle do Const 1 with {Const p r -> r 42 | return x -> return x }" `shouldBe` (Right (DNum 42))

    it "Identity effect handler" $ do
      evalProgram "handle do Id 42 with {Id p r -> return p | return x -> r 1 }" `shouldBe` (Right (DNum 42))

    it "Increment effect handler" $ do
      evalProgram "handle do Inc 17 with {Id p r -> return p | Inc p r -> return p + 1 | return x -> return 1 }" `shouldBe` (Right (DNum 18))

    it "Simulate exceptions" $ do
      evalProgram "handle let x <- do Except 42 in return 1 with {Except p r -> return p | return x -> return 1 }" `shouldBe` (Right (DNum 42))

    it "Compose let with do" $ do
      evalProgram "handle let x <- do Id 1 in return 3 with {Id p r -> r p | return x -> return x }" `shouldBe` (Right (DNum 3))

    it "Double increment effect handler" $ do
      evalProgram "handle let x <- do Inc 1 in do Inc x with {Inc p r -> let t <- return p + 1 in r t | return x -> return x }" `shouldBe` (Right (DNum 3))
