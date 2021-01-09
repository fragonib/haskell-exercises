module AdventOfCode.SlamShuffleSpec where

import qualified AdventOfCode.SlamShuffle as SUT

import Test.Hspec
import Test.QuickCheck

-- Specs

spec :: Spec
spec = do

  describe "Shuffle" $ do

    it "into new stack" $ do
      SUT.shuffleByRestacking [0..9] `shouldBe` [9,8,7,6,5,4,3,2,1,0]

    it "cut with positive position" $ do
      SUT.shuffleByCutting 3 [0..9] `shouldBe` [3,4,5,6,7,8,9,0,1,2]

    it "cut with negative position" $ do
      SUT.shuffleByCutting (-4) [0..9] `shouldBe` [6,7,8,9,0,1,2,3,4,5]

    it "deal with increment" $ do
      SUT.shuffleWithIncrement 3 [0..9] `shouldBe` [0,7,4,1,8,5,2,9,6,3]


  describe "Card locus" $ do

    it "find card after locus" $ do
      SUT.cardLocusAfterShuffling 2019 10007 [
         "deal with increment 73",
         "cut -8387",
         "deal with increment 41"
        ] `shouldBe` 2268
