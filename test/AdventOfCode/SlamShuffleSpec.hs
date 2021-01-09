module AdventOfCode.SlamShuffleSpec where

import Test.Hspec
import AdventOfCode.SlamShuffle
  (shuffleByRestackingPerm, shuffleByCuttingPerm, shuffleWithIncrementPerm,
   runPerm, cardLocusAfterShufflingPerm)

-- Specs

spec :: Spec
spec = do

  describe "Permutations" $ do

    it "by restacking" $ do
      runPerm (shuffleByRestackingPerm 10) 0 `shouldBe` 9
      runPerm (shuffleByRestackingPerm 10) 1 `shouldBe` 8
      runPerm (shuffleByRestackingPerm 10) 2 `shouldBe` 7
      runPerm (shuffleByRestackingPerm 10) 3 `shouldBe` 6
      runPerm (shuffleByRestackingPerm 10) 4 `shouldBe` 5
      runPerm (shuffleByRestackingPerm 10) 5 `shouldBe` 4
      runPerm (shuffleByRestackingPerm 10) 6 `shouldBe` 3
      runPerm (shuffleByRestackingPerm 10) 7 `shouldBe` 2
      runPerm (shuffleByRestackingPerm 10) 8 `shouldBe` 1
      runPerm (shuffleByRestackingPerm 10) 9 `shouldBe` 0

    it "by cutting positive" $ do
      runPerm (shuffleByCuttingPerm 10 3) 0 `shouldBe` 7
      runPerm (shuffleByCuttingPerm 10 3) 1 `shouldBe` 8
      runPerm (shuffleByCuttingPerm 10 3) 2 `shouldBe` 9
      runPerm (shuffleByCuttingPerm 10 3) 3 `shouldBe` 0
      runPerm (shuffleByCuttingPerm 10 3) 4 `shouldBe` 1
      runPerm (shuffleByCuttingPerm 10 3) 5 `shouldBe` 2
      runPerm (shuffleByCuttingPerm 10 3) 6 `shouldBe` 3
      runPerm (shuffleByCuttingPerm 10 3) 7 `shouldBe` 4
      runPerm (shuffleByCuttingPerm 10 3) 8 `shouldBe` 5
      runPerm (shuffleByCuttingPerm 10 3) 9 `shouldBe` 6

    it "by cutting negative" $ do
      runPerm (shuffleByCuttingPerm 10 (-4)) 0 `shouldBe` 4
      runPerm (shuffleByCuttingPerm 10 (-4)) 1 `shouldBe` 5
      runPerm (shuffleByCuttingPerm 10 (-4)) 2 `shouldBe` 6
      runPerm (shuffleByCuttingPerm 10 (-4)) 3 `shouldBe` 7
      runPerm (shuffleByCuttingPerm 10 (-4)) 4 `shouldBe` 8
      runPerm (shuffleByCuttingPerm 10 (-4)) 5 `shouldBe` 9
      runPerm (shuffleByCuttingPerm 10 (-4)) 6 `shouldBe` 0
      runPerm (shuffleByCuttingPerm 10 (-4)) 7 `shouldBe` 1
      runPerm (shuffleByCuttingPerm 10 (-4)) 8 `shouldBe` 2
      runPerm (shuffleByCuttingPerm 10 (-4)) 9 `shouldBe` 3

    it "with increment" $ do
      runPerm (shuffleWithIncrementPerm 10 3) 0 `shouldBe` 0
      runPerm (shuffleWithIncrementPerm 10 3) 1 `shouldBe` 3
      runPerm (shuffleWithIncrementPerm 10 3) 2 `shouldBe` 6
      runPerm (shuffleWithIncrementPerm 10 3) 3 `shouldBe` 9
      runPerm (shuffleWithIncrementPerm 10 3) 4 `shouldBe` 2
      runPerm (shuffleWithIncrementPerm 10 3) 5 `shouldBe` 5
      runPerm (shuffleWithIncrementPerm 10 3) 6 `shouldBe` 8
      runPerm (shuffleWithIncrementPerm 10 3) 7 `shouldBe` 1
      runPerm (shuffleWithIncrementPerm 10 3) 8 `shouldBe` 4
      runPerm (shuffleWithIncrementPerm 10 3) 9 `shouldBe` 7
      

  describe "Card locus" $ do

    it "find card after locus 1" $ do
      -- Start: 0 1 2 3 4 5 6 7 8 9
      -- End:   0 3 6 9 2 5 8 1 4 7
      runPerm (cardLocusAfterShufflingPerm 10 [
          "deal with increment 7",
          "deal into new stack",
          "deal into new stack"
       ]) 2 `shouldBe` 4

    it "find card after locus 2" $ do
      -- Start: 0 1 2 3 4 5 6 7 8 9
      -- Cut 6: 6 7 8 9 0 1 2 3 4 5
      -- End:   3 0 7 4 1 8 5 2 9 6
      runPerm (cardLocusAfterShufflingPerm 10 [
          "cut 6",
          "deal with increment 7",
          "deal into new stack"
       ]) 0 `shouldBe` 1

    it "find card after locus 3" $ do
      -- Start: 0 1 2 3 4 5 6 7 8 9
      -- End:   6 3 0 7 4 1 8 5 2 9
      runPerm (cardLocusAfterShufflingPerm 10 [
          "deal with increment 7",
          "deal with increment 9",
          "cut -2"
       ]) 2 `shouldBe` 8

    it "find card after locus 4" $ do
      -- Start: 0 1 2 3 4 5 6 7 8 9
      -- End:   9 2 5 8 1 4 7 0 3 6
      runPerm (cardLocusAfterShufflingPerm 10 [
          "deal into new stack",
          "cut -2",
          "deal with increment 7",
          "cut 8",
          "cut -4",
          "deal with increment 7",
          "cut 3",
          "deal with increment 9",
          "deal with increment 3",
          "cut -1"
       ]) 2 `shouldBe` 1

    it "find card after locus" $ do
      runPerm (cardLocusAfterShufflingPerm 10007 [
         "deal with increment 73",
         "cut -8387",
         "deal with increment 41"
        ]) 2019 `shouldBe` 2268