module AdventOfCode.SlamShuffleSpec where

import qualified AdventOfCode.SlamShuffle as SUT

import Test.Hspec
import Test.QuickCheck

-- Specs

spec :: Spec
spec = do

  describe "Shuffle" $ do

    it "into new stack" $ do
      SUT.restackDeck [0..9] `shouldBe` [9,8,7,6,5,4,3,2,1,0]

    it "cut with positive position" $ do
      SUT.cutDeck 3 [0..9] `shouldBe` [3,4,5,6,7,8,9,0,1,2]

    it "cut with negative position" $ do
      SUT.cutDeck (-4) [0..9] `shouldBe` [6,7,8,9,0,1,2,3,4,5]

    it "deal with increment" $ do
      SUT.dealWithIncrement 3 [0..9] `shouldBe` [0,7,4,1,8,5,2,9,6,3]