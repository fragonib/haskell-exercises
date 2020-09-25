module Kata.KnightsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.Knights as SUT


spec :: Spec
spec = do

  describe "knight movements" $ do

    it "when top-left position" $ do
      SUT.knightMovement (0, 0) `shouldBe` [(1,2),(2,1)]
      
    it "when down-rigth position" $ do
      SUT.knightMovement (4, 4) `shouldBe` [(2,3),(3,2)]
      
    it "when middle position" $ do
      SUT.knightMovement (2, 2) `shouldBe` [(0,1),(0,3),(1,0),(1,4),(3,0),(3,4),(4,1),(4,3)]
