module HaskellProgramming.MonoidSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import HaskellProgramming.Monoid
import HaskellProgramming.SemiGroup (semigroupAssociativity)
import Data.Monoid 

spec :: Spec
spec = do

  describe "Monoid laws for String Monoid" $ do

    it "check associativity" $ do
      quickCheck (semigroupAssociativity :: String -> String -> String -> Bool)
      -- verboseCheck (monoidAssociativity :: String -> String -> String -> Bool)

    it "check identity" $ do
      quickCheck (monoidLeftIdentity :: String -> Bool)
      quickCheck (monoidRightIdentity :: String -> Bool)

  describe "Monoid instances examples" $ do

    it "Opcional monoid" $ do
      Valor (Sum 1) `mappend` Valor (Sum 1) `shouldBe` Valor (Sum 2)
      Valor (Product 4) `mappend` Valor (Product 2) `shouldBe` Valor (Product 8)
      Valor (Sum 1) `mappend` Nada `shouldBe` Valor (Sum 1)
      Valor [1] `mappend` Nada `shouldBe` Valor [1]
      Nada `mappend` Valor (Sum 1) `shouldBe` Valor (Sum 1)
