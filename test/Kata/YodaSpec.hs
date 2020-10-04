module Kata.YodaSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.Yoda as SUT


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

  describe "Collision" $ do

    it "sample 1" $ do
      SUT.yodaCLI "300" "500" `shouldBe` "0"
      SUT.yodaCLI "500" "300" `shouldBe` "500"

    it "sample 2" $ do
      SUT.yodaCLI "65743" "9651" `shouldBe` "673"
      SUT.yodaCLI "9651" "65743" `shouldBe` "95"

    it "sample 3" $ do
      SUT.yodaCLI "2341" "6785" `shouldBe` "YODA"
      SUT.yodaCLI "6785" "2341" `shouldBe` "6785"

