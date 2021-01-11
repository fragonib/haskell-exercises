module AdventOfCode.SlamShuffle2Spec where

import Test.Hspec
import AdventOfCode.SlamShuffle2

-- Specs

spec :: Spec
spec = do

  describe "Small deck" $ do

    it "mixing shuffles 1" $ do
      -- Start: 0 1 2 3 4 5 6 7 8 9
      -- End:   0 3 6 9 2 5 8 1 4 7
      let perm = slamShuffle2 10 1 [
                    "deal with increment 7",
                    "deal into new stack",
                    "deal into new stack"
                 ]
       in map (runPerm perm 10) [0..9] `shouldBe` [0,7,4,1,8,5,2,9,6,3]

    it "mixing shuffles 2" $ do
      -- Start: 0 1 2 3 4 5 6 7 8 9
      -- Cut 6: 6 7 8 9 0 1 2 3 4 5
      -- Inc 7: 6 9 2 5 8 1 4 7 0 3
      -- End:   3 0 7 4 1 8 5 2 9 6
      let perm = slamShuffle2 10 1 [
                    "cut 6",
                    "deal with increment 7",
                    "deal into new stack"
                 ]
       in map (runPerm perm 10) [0..9] `shouldBe` [1,4,7,0,3,6,9,2,5,8]

    it "mixing shuffles 3" $ do
      -- Start:  0 1 2 3 4 5 6 7 8 9
      -- Int 7:  0 3 6 9 2 5 8 1 4 7
      -- Int 9:  0 7 4 1 8 5 2 9 6 3
      -- Cut -2: 6 3 0 7 4 1 8 5 2 9
      let perm = slamShuffle2 10 1 [
                    "deal with increment 7",
                    "deal with increment 9",
                    "cut -2"
                 ]
       in map (runPerm perm 10) [0..9] `shouldBe` [2,5,8,1,4,7,0,3,6,9]

    it "mixing shuffles 4" $ do
      -- Start: 0 1 2 3 4 5 6 7 8 9
      -- End:   9 2 5 8 1 4 7 0 3 6
      let perm = slamShuffle2 10 1 [
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
                 ]
       in map (runPerm perm 10) [0..9] `shouldBe` [7,4,1,8,5,2,9,6,3,0]
       

  describe "Huge deck" $ do

    it "only one card location" $ do
      runPerm (slamShuffle2 119315717514047 101741582076661 [
          "deal with increment 73",
          "cut -8387",
          "deal with increment 41"
        ]) 119315717514047 2020 `shouldBe` 23789870559116