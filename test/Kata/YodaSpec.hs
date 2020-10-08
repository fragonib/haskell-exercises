module Kata.YodaSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.Yoda as SUT


spec :: Spec
spec = do

  describe "Yoda Int Collision" $ do

    it "sample 1" $ do
      SUT.yodaCLI "300" "500" `shouldBe` "0"
      SUT.yodaCLI "500" "300" `shouldBe` "500"

    it "sample 2" $ do
      SUT.yodaCLI "65743" "9651" `shouldBe` "673"
      SUT.yodaCLI "9651" "65743" `shouldBe` "95"

    it "sample 3" $ do
      SUT.yodaCLI "2341" "6785" `shouldBe` "YODA"
      SUT.yodaCLI "6785" "2341" `shouldBe` "6785"

