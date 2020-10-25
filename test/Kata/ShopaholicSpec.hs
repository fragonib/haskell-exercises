module Kata.ShopaholicSpec where

import Test.Hspec
import qualified Kata.Shopaholic as SUT


spec :: Spec
spec = do

  describe "Total discount" $ do

    it "sample1" $ do
      SUT.totalDiscount [400,100,200,350,300,250] `shouldBe` 400
          
    it "sample2" $ do
      SUT.totalDiscount [400, 350, 300, 250, 200, 150, 100] `shouldBe` 450
