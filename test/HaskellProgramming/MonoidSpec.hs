module HaskellProgramming.MonoidSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception ()
import HaskellProgramming.SemiGroup
import HaskellProgramming.Monoid
import Data.Monoid 

-- Shortcuts 

type TwoSumInt = Two (Sum Int) (Sum Int)
type ThreeSumInt = Three (Sum Int) (Sum Int) (Sum Int)
type FourSumInt = Four (Sum Int) (Sum Int) (Sum Int) (Sum Int)


spec :: Spec
spec = do

  describe "Behaviour" $ do

    it "Opcional monoid" $ do
      Valor (Sum 1) `mappend` Valor (Sum 1) `shouldBe` Valor (Sum 2)
      Valor (Product 4) `mappend` Valor (Product 2) `shouldBe` Valor (Product 8)
      Valor (Sum 1) `mappend` Nada `shouldBe` Valor (Sum 1)
      Valor [1] `mappend` Nada `shouldBe` Valor [1]
      Nada `mappend` Valor (Sum 1) `shouldBe` Valor (Sum 1)

  describe "Laws verification" $ do
    
    it "String" $ do
      quickCheck (monoidLeftIdentity :: String -> Bool)
      quickCheck (monoidRightIdentity :: String -> Bool)

    it "Trivial" $ do
      quickCheck (monoidLeftIdentity :: Trivial -> Bool)
      quickCheck (monoidRightIdentity :: Trivial -> Bool)

    it "Identity" $ do
      quickCheck (monoidLeftIdentity :: Identity (Sum Int) -> Bool)
      quickCheck (monoidRightIdentity :: Identity (Sum Int) -> Bool)

    it "Two" $ do
      quickCheck (monoidLeftIdentity :: TwoSumInt -> Bool)
      quickCheck (monoidRightIdentity :: TwoSumInt -> Bool)

    it "Three" $ do
      quickCheck (monoidLeftIdentity :: ThreeSumInt -> Bool)
      quickCheck (monoidRightIdentity :: ThreeSumInt -> Bool)

    it "Four" $ do
      quickCheck (monoidLeftIdentity :: FourSumInt -> Bool)
      quickCheck (monoidRightIdentity :: FourSumInt -> Bool)

--    it "BoolConj" $ do
--      quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
--      quickCheck (monoidRightIdentity :: BoolConj -> Bool)
--
--    it "BoolDisj" $ do
--      quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
--      quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
--
--    it "Special 'Or'" $ do
--      quickCheck (monoidLeftIdentity :: Or Int Int -> Bool)
--      quickCheck (monoidRightIdentity :: Or Int Int -> Bool)
--
--    it "Compose" $ do
--      True `shouldBe` True
--    
--    it "Combine" $ do
--      True `shouldBe` True
--
--    it "Validation" $ do
--      quickCheck (monoidLeftIdentity :: Validation String Int -> Validation String Int -> Bool)
--      quickCheck (monoidRightIdentity :: Validation String Int -> Validation String Int -> Bool)
--
--    it "AccumulateRight" $ do
--      quickCheck (monoidLeftIdentity :: AccumulateRight String (Sum Int) -> Bool)
--      quickCheck (monoidRightIdentity :: AccumulateRight String (Sum Int) -> Bool)
--
--    it "AccumulateBoth" $ do
--      quickCheck (monoidLeftIdentity :: AccumulateBoth String (Sum Int) -> Bool)
--      quickCheck (monoidRightIdentity :: AccumulateBoth String (Sum Int) -> Bool)
--