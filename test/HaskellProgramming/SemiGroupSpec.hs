module HaskellProgramming.SemiGroupSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception ()
import HaskellProgramming.SemiGroup
import Data.Monoid


spec :: Spec
spec = do

  describe "Semigroup law verificaction" $ do

    it "Trivial" $ do
      quickCheck (semigroupAssociativity :: Trivial -> Trivial -> Trivial -> Bool)

    it "Identity" $ do
      quickCheck (semigroupAssociativity :: Identity Int -> Identity Int -> Identity Int -> Bool)

    it "Two" $ do
      quickCheck (semigroupAssociativity :: Two Int Int -> Two Int Int -> Two Int Int -> Bool)

    it "Three" $ do
      quickCheck (semigroupAssociativity :: Three Int Int Int -> Three Int Int Int -> Three Int Int Int -> Bool)

    it "Four" $ do
      quickCheck (semigroupAssociativity :: Four Int Int Int Int -> Four Int Int Int Int -> Four Int Int Int Int -> Bool)

    it "BoolConj behaviour" $ do
      BoolConj True <> BoolConj True `shouldBe` BoolConj True
      BoolConj True <> BoolConj False `shouldBe` BoolConj False

    it "BoolConj laws" $ do
      quickCheck (semigroupAssociativity :: BoolConj -> BoolConj -> BoolConj -> Bool)

    it "BoolDisj behaviour" $ do
      BoolDisj True <> BoolDisj True `shouldBe` BoolDisj True
      BoolDisj True <> BoolDisj False `shouldBe` BoolDisj True

    it "BoolDisj laws" $ do
      quickCheck (semigroupAssociativity :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)

    it "Snd preferrent 'Or' behaviour" $ do
      Fst 1 <> Snd 2 `shouldBe` (Snd 2::Or Int Int)
      Fst 1 <> Fst 2 `shouldBe` (Fst 2::Or Int Int)
      Snd 1 <> Fst 2 `shouldBe` (Snd 1::Or Int Int)
      Snd 1 <> Snd 2 `shouldBe` (Snd 1::Or Int Int)

    it "Special 'Or' laws" $ do
      quickCheck (semigroupAssociativity :: Or Int Int -> Or Int Int -> Or Int Int -> Bool)

    it "Combine behaviour" $ do
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

    it "Compose behaviour" $ do
      let f = Compose $ \n -> n + 1 :: Int
          g = Compose $ \n -> n - 1
       in unCompose (f <> g) 0 `shouldBe` 0

      let f = Compose $ \n -> n + 1 :: Int
       in unCompose (f <> f) 1 `shouldBe` 3

      let f = Compose $ \n -> n + 1 :: Int
          g = Compose $ \n -> n - 1
       in unCompose (g <> f) 1 `shouldBe` 1

    it "Validation laws" $ do
      quickCheck (semigroupAssociativity :: Validation String Int -> Validation String Int -> Validation String Int -> Bool)

    it "AccumulateRight laws" $ do
      quickCheck (semigroupAssociativity :: AccumulateRight String (Sum Int) -> AccumulateRight String (Sum Int) -> AccumulateRight String (Sum Int) -> Bool)

    it "AccumulateBoth laws" $ do
      quickCheck (semigroupAssociativity :: AccumulateBoth String (Sum Int) -> AccumulateBoth String (Sum Int) -> AccumulateBoth String (Sum Int) -> Bool)

