module Kata.BackslashSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.Backslash as SUT


spec :: Spec
spec = do

  describe "Special" $ do

    it "isSpecial" $ do
      SUT.isSpecial '2' `shouldBe` False
      SUT.isSpecial '!' `shouldBe` True
      SUT.isSpecial '*' `shouldBe` True
      SUT.isSpecial '[' `shouldBe` True
      SUT.isSpecial ']' `shouldBe` True
      

  describe "IO" $ do

    it "sample 1" $ do

      SUT.backslashCLI [
        "1",
        "this is a 'test'",
        "2",
        "/:-)"
        ]
      `shouldBe` [
        -- Haskell itself requires its own level of interpretation
        "this is a \\'test\\'",
        "/:-\\\\\\)"
        ]