module HaskellProgramming.AgnosticMonoidSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception ()
import HaskellProgramming.SemiGroup (Opcional (Nada), Opcional (Valor), semigroupAssociativity)
import HaskellProgramming.Monoid (monoidLeftIdentity, monoidRightIdentity)
import HaskellProgramming.AgnosticMonoid


spec :: Spec
spec = do

  describe "Agnostic Monoid instance" $ do
    
    describe "Laws" $ do
  
      it "Associativity" $ do
        property (semigroupAssociativity :: OrLikeMonoid String -> OrLikeMonoid String -> OrLikeMonoid String -> Bool)
  
      it "Left identity" $ do
        property (monoidLeftIdentity :: OrLikeMonoid String -> Bool)
  
      it "Right identity" $ do
        property (monoidRightIdentity :: OrLikeMonoid String -> Bool)
  
    describe "Behaviour" $ do
      
      it "Or like and left-valued" $ do
        OrLikeMonoid Nada <> OrLikeMonoid (Nada::Opcional Int) `shouldBe` OrLikeMonoid Nada
        OrLikeMonoid Nada <> OrLikeMonoid (Valor 2::Opcional Int) `shouldBe` OrLikeMonoid (Valor 2)
        OrLikeMonoid (Valor 1) <> OrLikeMonoid (Nada::Opcional Int) `shouldBe` OrLikeMonoid (Valor 1)
        OrLikeMonoid (Valor 1) <> OrLikeMonoid (Valor 2::Opcional Int) `shouldBe` OrLikeMonoid (Valor 1)
