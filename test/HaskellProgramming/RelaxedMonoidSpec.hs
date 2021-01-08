module HaskellProgramming.RelaxedMonoidSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception ()
import HaskellProgramming.SemiGroup (Opcional (Nada), Opcional (Valor), semigroupAssociativity)
import HaskellProgramming.Monoid (monoidLeftIdentity, monoidRightIdentity)
import HaskellProgramming.RelaxedMonoid


spec :: Spec
spec = do

  describe "Agnostic Monoid instance" $ do
    
    describe "Behaviour" $ do
      
      it "Or like and left-valued" $ do
        LeftBiasedOpcionalConj Nada <> LeftBiasedOpcionalConj Nada `shouldBe` LeftBiasedOpcionalConj (Nada :: Opcional Int)
        LeftBiasedOpcionalConj Nada <> LeftBiasedOpcionalConj (Valor 2) `shouldBe` LeftBiasedOpcionalConj (Valor 2 :: Opcional Int)
        LeftBiasedOpcionalConj (Valor 1) <> LeftBiasedOpcionalConj Nada `shouldBe` LeftBiasedOpcionalConj (Valor 1 :: Opcional Int)
        LeftBiasedOpcionalConj (Valor 1) <> LeftBiasedOpcionalConj (Valor 2) `shouldBe` LeftBiasedOpcionalConj (Valor 1 :: Opcional Int)

    describe "Laws" $ do
  
      it "Associativity" $ do
        property (semigroupAssociativity :: LeftBiasedOpcionalConj Int -> LeftBiasedOpcionalConj Int -> LeftBiasedOpcionalConj Int -> Bool)
  
      it "Left identity" $ do
        property (monoidLeftIdentity :: LeftBiasedOpcionalConj Int -> Bool)
  
      it "Right identity" $ do
        property (monoidRightIdentity :: LeftBiasedOpcionalConj Int -> Bool)