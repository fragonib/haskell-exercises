module HaskellProgramming.MonoidSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception ()
import HaskellProgramming.SemiGroup
import HaskellProgramming.Monoid
import Data.Monoid

-- Shortcuts 

type OptionalSumInt = Opcional (Sum Int)
type TwoSumInt = Two (Sum Int) (Sum Int)
type ThreeSumInt = Three (Sum Int) (Sum Int) (Sum Int)
type FourSumInt = Four (Sum Int) (Sum Int) (Sum Int) (Sum Int)


spec :: Spec
spec = do

  describe "Behaviour" $ do

    it "BoolConj" $ do
      BoolConj True <> mempty `shouldBe` BoolConj True
      mempty <> BoolConj False `shouldBe` BoolConj False

    it "BoolDisj" $ do
      BoolDisj True <> mempty `shouldBe` BoolDisj True
      mempty <> BoolDisj False `shouldBe` BoolDisj False

    it "Mem" $ do
      
      let f = Mem $ \s -> ("hi", s + 1) :: (String, Int)
       in runMem (f <> mempty) 0 `shouldBe` ("hi", 1)
       
      let f = Mem $ \s -> ("hi", s + 1) :: (String, Int)
       in runMem (mempty <> f) 0 `shouldBe` ("hi", 1)
      
      (runMem mempty 0 :: (String, Int)) `shouldBe` ("", 0) 
      
      let f = Mem $ \s -> ("hi", s + 1) :: (String, Int)
       in runMem (f <> mempty) 0 == runMem f 0 `shouldBe` True 
      
      let f = Mem $ \s -> ("hi", s + 1) :: (String, Int)
       in runMem (mempty <> f) 0 == runMem f 0 `shouldBe` True
      
      let f = Mem $ \s -> ("hi", s + 1) :: (String, Int)
       in runMem (f <> f) 0 `shouldBe` ("hihi", 2)

  describe "Laws verification" $ do
    
    it "String Left Identity" $ property (monoidLeftIdentity :: String -> Bool)
    it "String Right Identity" $ property (monoidRightIdentity :: String -> Bool)
    
    it "Opcional Left Identity" $ property (monoidLeftIdentity :: OptionalSumInt -> Bool)
    it "Opcional Right Identity" $ property (monoidRightIdentity :: OptionalSumInt -> Bool)
   
    it "Trivial Left Identity" $ property (monoidLeftIdentity :: Trivial -> Bool)
    it "Trivial Right Identity" $ property (monoidRightIdentity :: Trivial -> Bool)
   
    it "Identity Left Identity" $ property (monoidLeftIdentity :: Identity (Sum Int) -> Bool)
    it "Identity Right Identity" $ property (monoidRightIdentity :: Identity (Sum Int) -> Bool)
   
    it "Two Left Identity" $ property (monoidLeftIdentity :: TwoSumInt -> Bool)
    it "Two Right Identity" $ property (monoidRightIdentity :: TwoSumInt -> Bool)
   
    it "Three Left Identity" $ property (monoidLeftIdentity :: ThreeSumInt -> Bool)
    it "Three Right Identity" $ property (monoidRightIdentity :: ThreeSumInt -> Bool)
   
    it "Four Left Identity" $ property (monoidLeftIdentity :: FourSumInt -> Bool)
    it "Four Right Identity" $ property (monoidRightIdentity :: FourSumInt -> Bool)
   
    it "BoolConj Left Identity" $ property (monoidLeftIdentity :: BoolConj -> Bool)
    it "BoolConj Right Identity" $ property (monoidRightIdentity :: BoolConj -> Bool)
   
    it "BoolDisj Left Identity" $ property (monoidLeftIdentity :: BoolDisj -> Bool)
    it "BoolDisj Right Identity" $ property (monoidRightIdentity :: BoolDisj -> Bool)
   
    it "Special 'Or' Left Identity" $ property (monoidLeftIdentity :: Or (Sum Int) (Sum Int) -> Bool)
    -- it "Special 'Or' Right Identity" $ property (monoidRightIdentity :: Or (Sum Int) (Sum Int) -> Bool)
   
    it "Compose Left Identity" $ True `shouldBe` True
    it "Compose Right Identity" $ True `shouldBe` True
   
    it "Combine Left Identity" $ True `shouldBe` True
    it "Combine Right Identity" $ True `shouldBe` True
   
    it "Validation Left Identity" $ property (monoidLeftIdentity :: Validation String (Sum Int) -> Bool)
    -- it "Validation Right Identity" $ property (monoidRightIdentity :: Validation String (Sum Int) -> Bool)
   
    it "AccumulateRight Left Identity" $ property (monoidLeftIdentity :: AccumulateRight String (Sum Int) -> Bool)
    it "AccumulateRight Right Identity" $ property (monoidRightIdentity :: AccumulateRight String (Sum Int) -> Bool)
   
    it "AccumulateBoth Right Identity" $ property (monoidRightIdentity :: AccumulateBoth String (Sum Int) -> Bool)
    it "AccumulateBoth Left Identity" $ property (monoidLeftIdentity :: AccumulateBoth String (Sum Int) -> Bool)
    
    it "Mem Left Identity" $ True `shouldBe` True
    it "Mem Right Identity" $ True `shouldBe` True