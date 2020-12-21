module HaskellProgramming.Chapter15Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import HaskellProgramming.Chapter15
import Data.Monoid 

spec :: Spec
spec = do

  describe "monoid" $ do

    it "optional" $ do
      Only (Sum 1) `mappend` Only (Sum 1) `shouldBe` Only (Sum 2)
      Only (Product 4) `mappend` Only (Product 2) `shouldBe` Only (Product 8)
      Only (Sum 1) `mappend` Nada `shouldBe` Only (Sum 1)
      Only [1] `mappend` Nada `shouldBe` Only [1]
      Nada `mappend` Only (Sum 1) `shouldBe` Only (Sum 1)
      
  describe "String monoid properties" $ do
    
    it "associativity" $ do
      quickCheck (monoidAssociativity :: String -> String -> String -> Bool)      
      verboseCheck (monoidAssociativity :: String -> String -> String -> Bool)      
    
    it "identity" $ do
      quickCheck (monoidLeftIdentity :: String -> Bool)      
      quickCheck (monoidRightIdentity :: String -> Bool)      


