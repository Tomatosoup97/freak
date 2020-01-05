{- HLINT ignore -}
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Freak
import Eval
import Types

genRow l n r = DPair (DLabel l) (DPair (DNum n) r)

main :: IO ()
main = hspec $ do
  let row = "(a = 1; (b = 2; ()))"
  let row' = "(c = 3; " ++ row ++ ")"
  let variant s = "["++s++" 1 ] : row"

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

    it "Let expression" $ do
      evalProgram "let x <- return 3 in return x + 2" `shouldBe` (Right (DNum 5))
      evalProgram "let x <- return x in return x + 2" `shouldBe` (unboundVarErr "x")

    it "Row" $ do
      evalProgram ("return "++row) `shouldBe` (Right (genRow "a" 1 (genRow "b" 2 DUnit)))

    it "Split" $ do
      evalProgram ("let (b = x; y) = "++row'++" in return 1 + x") `shouldBe` (Right (DNum 3))
      evalProgram ("let (a = x; y) = "++row'++" in return 1 + x") `shouldBe` (Right (DNum 2))

    it "Case" $ do
      let body = " {a x -> return x; y -> return 42}"
      evalProgram ("case "++variant "a"++body) `shouldBe` (Right (DNum 1))
      evalProgram ("case "++variant "c"++body) `shouldBe` (Right (DNum 42))

    it "Lambda application" $ do
      evalProgram "(\\x : int -> return x + 1) 42" `shouldBe` (Right (DNum 43))
