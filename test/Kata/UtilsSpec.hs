module Kata.UtilsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.Utils as SUT


spec :: Spec
spec = do

  describe "Padding" $ do

    it "leftPadZero" $ do
      SUT.leftPadZero 5 [] `shouldBe` [0,0,0,0,0]
      SUT.leftPadZero 5 [1,2,3] `shouldBe` [0,0,1,2,3]
      SUT.leftPadZero 3 [1,2,3] `shouldBe` [1,2,3]
      SUT.leftPadZero 2 [1,2,3] `shouldBe` [1,2,3]

    it "leftTruncateZero" $ do
      SUT.leftTruncateZero [] `shouldBe` []
      SUT.leftTruncateZero [1,2,3] `shouldBe` [1,2,3]
      SUT.leftTruncateZero [0,2,3] `shouldBe` [2,3]
      SUT.leftTruncateZero [0,0,3] `shouldBe` [3]
      SUT.leftTruncateZero [0,0,0] `shouldBe` [0]
      
  describe "Tracing" $ do

    it "trace 2" $ do
      SUT.trace2 (+) 5 2 `shouldBe` 7