module Kata.SortSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.Sort as SUT


spec :: Spec
spec = do

  describe "Sort" $ do

    it "sample1" $ do
      SUT.sort [2, 1, 2, 1, 2] `shouldBe` [2, 2, 2, 1, 1]

    it "sample2" $ do
      SUT.sort [1,3,3,3,2,2,2,1,1] `shouldBe` [1,1,1,3,3,3,2,2,2]

    it "sample3" $ do
        SUT.sort [11,33,11,77,54,11,25,25,33] `shouldBe` [11,11,11,33,33,25,25,77,54]
