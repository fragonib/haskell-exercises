module HaskellProgramming.SemiGroupSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception ()
import HaskellProgramming.SemiGroup
import Data.Monoid

-- Shortcuts 

type OptionalSumInt = Opcional (Sum Int)
type IdentitySumInt = Identity (Sum Int)
type TwoSumInt = Two (Sum Int) (Sum Int)
type ThreeSumInt = Three (Sum Int) (Sum Int) (Sum Int)
type FourSumInt = Four (Sum Int) (Sum Int) (Sum Int) (Sum Int)

-- Specs

spec :: Spec
spec = do

  describe "Behaviour" $ do
    
    it "Opcional monoid" $ do
      Valor (Sum 1) <> Valor (Sum 1) `shouldBe` Valor (Sum 2)
      Valor (Product 4) <> Valor (Product 2) `shouldBe` Valor (Product 8)
      Valor (Sum 1) <> Nada `shouldBe` Valor (Sum 1)
      Valor [1] <> Nada `shouldBe` Valor [1]
      Nada <> Valor (Sum 1) `shouldBe` Valor (Sum 1)
    
    it "BoolConj" $ do
      BoolConj True <> BoolConj True `shouldBe` BoolConj True
      BoolConj True <> BoolConj False `shouldBe` BoolConj False
    
    it "BoolDisj" $ do
      BoolDisj True <> BoolDisj True `shouldBe` BoolDisj True
      BoolDisj True <> BoolDisj False `shouldBe` BoolDisj True

    it "Or (is Snd preferrent, and left-valued) " $ do
      Fst 1 <> Snd 2 `shouldBe` (Snd 2 :: Or Int Int)
      Fst 1 <> Fst 2 `shouldBe` (Fst 2 :: Or Int Int)
      Snd 1 <> Fst 2 `shouldBe` (Snd 1 :: Or Int Int)
      Snd 1 <> Snd 2 `shouldBe` (Snd 1 :: Or Int Int)
      
    it "Combine" $ do
      let f = Combine $ \n -> Sum (n + 1) :: Sum Int
          g = Combine $ \n -> Sum (n - 1) :: Sum Int
       in unCombine (f <> g) 0 `shouldBe` (Sum 0 :: Sum Int)

      let f = Combine $ \n -> Sum (n + 1) :: Sum Int
          g = Combine $ \n -> Sum (n - 1) :: Sum Int
       in unCombine (f <> g) 1 `shouldBe` (Sum 2 :: Sum Int)

      let f = Combine $ \n -> Sum (n + 1) :: Sum Int
       in unCombine (f <> f) 1 `shouldBe` (Sum 4 :: Sum Int)

      let f = Combine $ \n -> Sum (n + 1) :: Sum Int
          g = Combine $ \n -> Sum (n - 1) :: Sum Int
       in unCombine (g <> f) 1 `shouldBe` (Sum 2 :: Sum Int)
       
    it "Compose" $ do
      let f = Compose $ \n -> n + 1 :: Int
          g = Compose $ \n -> n - 1
       in unCompose (f <> g) 0 `shouldBe` 0

      let f = Compose $ \n -> n + 1 :: Int
       in unCompose (f <> f) 1 `shouldBe` 3

      let f = Compose $ \n -> n + 1 :: Int
          g = Compose $ \n -> n - 1
       in unCompose (g <> f) 1 `shouldBe` 1
       
  
  describe "Laws verificaction" $ do
  
    it "String" $ do
      property (semigroupAssociativity :: String -> String -> String -> Bool)
      -- verboseCheck (monoidAssociativity :: String -> String -> String -> Bool)

    it "Opcional" $ do
      property (semigroupAssociativity :: OptionalSumInt -> OptionalSumInt -> OptionalSumInt -> Bool)

    it "Trivial" $ do
      property (semigroupAssociativity :: Trivial -> Trivial -> Trivial -> Bool)

    it "Identity" $ do
      property (semigroupAssociativity :: IdentitySumInt -> IdentitySumInt -> IdentitySumInt -> Bool)

    it "Two" $ do
      property (semigroupAssociativity :: TwoSumInt -> TwoSumInt -> TwoSumInt -> Bool)

    it "Three" $ do
      property (semigroupAssociativity :: ThreeSumInt -> ThreeSumInt -> ThreeSumInt -> Bool)

    it "Four" $ do
      property (semigroupAssociativity :: FourSumInt -> FourSumInt -> FourSumInt -> Bool)

    it "BoolConj" $ do
      property (semigroupAssociativity :: BoolConj -> BoolConj -> BoolConj -> Bool)

    it "BoolDisj" $ do
      property (semigroupAssociativity :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)

    it "Special 'Or'" $ do
      property (semigroupAssociativity :: Or Int Int -> Or Int Int -> Or Int Int -> Bool)

    it "Compose" $ do
      True `shouldBe` True

    it "Combine" $ do
      True `shouldBe` True
      -- property (semigroupAssociativity :: Combine Int (Sum Int) -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Bool)

    it "Validation" $ do
      property (semigroupAssociativity :: Validation String Int -> Validation String Int -> Validation String Int -> Bool)

    it "AccumulateRight" $ do
      property (semigroupAssociativity :: AccumulateRight String (Sum Int) -> AccumulateRight String (Sum Int) -> AccumulateRight String (Sum Int) -> Bool)

    it "AccumulateBoth" $ do
      property (semigroupAssociativity :: AccumulateBoth String (Sum Int) -> AccumulateBoth String (Sum Int) -> AccumulateBoth String (Sum Int) -> Bool)

    it "Mem" $ 
      True `shouldBe` True
