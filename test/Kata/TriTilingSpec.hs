module Kata.TriTilingSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.TriTiling as SUT


spec :: Spec
spec = do

  describe "find solution" $ do

    it "sample 2" $ do
      SUT.f 2 `shouldBe` 3

    it "sample 4" $ do
      SUT.f 4 `shouldBe` 11

    it "sample 6" $ do
      SUT.f 6 `shouldBe` 41

    it "sample 8" $ do
      SUT.f 8 `shouldBe` 153

    it "sample 12" $ do
      SUT.f 12 `shouldBe` 2131
