module HaskellProgramming.AgnosticMonoidSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception ()
import HaskellProgramming.Monoid (Opcional (Nada), Opcional (Valor), monoidAssociativity, monoidLeftIdentity, monoidRightIdentity)
import HaskellProgramming.AgnosticMonoid


spec :: Spec
spec = do

  describe "Monoid laws for Agnotic Monoid" $ do

    it "check associativity" $ do
      quickCheck (monoidAssociativity :: OrLikeMonoid String -> OrLikeMonoid String -> OrLikeMonoid String -> Bool)

    it "check identity" $ do
      quickCheck (monoidLeftIdentity :: OrLikeMonoid String -> Bool)
      quickCheck (monoidRightIdentity :: OrLikeMonoid String -> Bool)

  describe "Agnostic Monoid intance" $ do
    
    it "Behaviour" $ do
      
      OrLikeMonoid Nada `mappend` OrLikeMonoid (Nada::Opcional Int)
        `shouldBe` OrLikeMonoid Nada

      OrLikeMonoid Nada `mappend` OrLikeMonoid (Valor 2::Opcional Int)
        `shouldBe` OrLikeMonoid (Valor 2)

      OrLikeMonoid (Valor 1) `mappend` OrLikeMonoid (Nada::Opcional Int)
        `shouldBe` OrLikeMonoid (Valor 1)

      OrLikeMonoid (Valor 1) `mappend` OrLikeMonoid (Valor 2::Opcional Int)
        `shouldBe` OrLikeMonoid (Valor 1)
        
