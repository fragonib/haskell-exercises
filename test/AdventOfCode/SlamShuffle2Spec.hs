module AdventOfCode.SlamShuffle2Spec where

import AdventOfCode.SlamShuffle2
import Test.Hspec
import Kata.Utils


-- Specs

spec :: Spec
spec = do

  describe "Small deck & few shuffles" $ do

    it "mixing shuffles 1" $ do
      -- Start: 0 1 2 3 4 5 6 7 8 9
      -- End:   0 3 6 9 2 5 8 1 4 7
      let finalPerm = slamShuffle2 10 1 [
                    "deal with increment 7",
                    "deal into new stack",
                    "deal into new stack"
                 ]
--       in print finalPerm
       in map (runPerm finalPerm 10) [0..9] `shouldBe` [0,7,4,1,8,5,2,9,6,3]

    it "mixing shuffles 2" $ do
      -- Start: 0 1 2 3 4 5 6 7 8 9
      -- Cut 6: 6 7 8 9 0 1 2 3 4 5
      -- Inc 7: 6 9 2 5 8 1 4 7 0 3
      -- End:   3 0 7 4 1 8 5 2 9 6
      let finalPerm = slamShuffle2 10 1 [
                    "cut 6",
                    "deal with increment 7",
                    "deal into new stack"
                 ]
--       in print finalPerm
       in map (runPerm finalPerm 10) [0..9] `shouldBe` [1,4,7,0,3,6,9,2,5,8]

    it "mixing shuffles 3" $ do
      -- Start:  0 1 2 3 4 5 6 7 8 9
      -- Int 7:  0 3 6 9 2 5 8 1 4 7
      -- Int 9:  0 7 4 1 8 5 2 9 6 3
      -- Cut -2: 6 3 0 7 4 1 8 5 2 9
      let finalPerm = slamShuffle2 10 1 [
                    "deal with increment 7",
                    "deal with increment 9",
                    "cut -2"
                 ]
--       in print finalPerm
       in map (runPerm finalPerm 10) [0..9] `shouldBe` [2,5,8,1,4,7,0,3,6,9]

    it "mixing shuffles 4" $ do
      -- Start: 0 1 2 3 4 5 6 7 8 9
      -- End:   9 2 5 8 1 4 7 0 3 6
      let finalPerm = slamShuffle2 10 1 [
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
--         in print finalPerm
       in map (runPerm finalPerm 10) [0..9] `shouldBe` [7,4,1,8,5,2,9,6,3,0]
       

  describe "Huge deck" $ do

    it "origin of only one card" $ do
       invertPerm finalPerm hugePileSize 2020 `shouldBe` 41653717360577
       where
         finalPerm = slamShuffle2 hugePileSize hugeRepetitions shuffleSequence
         hugePileSize = 119315717514047
         hugeRepetitions = 101741582076661
         shuffleSequence = [
            "cut 181",
            "deal with increment 61",
            "cut -898",
            "deal with increment 19",
            "cut -1145",
            "deal with increment 35",
            "cut 3713",
            "deal with increment 8",
            "deal into new stack",
            "cut -168",
            "deal with increment 32",
            "cut -3050",
            "deal with increment 74",
            "cut 7328",
            "deal with increment 38",
            "deal into new stack",
            "deal with increment 11",
            "cut 5419",
            "deal with increment 34",
            "cut 7206",
            "deal with increment 53",
            "cut 4573",
            "deal into new stack",
            "deal with increment 50",
            "cut -1615",
            "deal with increment 9",
            "cut -4772",
            "deal with increment 66",
            "cut 9669",
            "deal into new stack",
            "deal with increment 2",
            "cut 5003",
            "deal with increment 46",
            "cut -3368",
            "deal into new stack",
            "cut 1276",
            "deal with increment 19",
            "cut 530",
            "deal with increment 57",
            "cut 8914",
            "deal with increment 41",
            "cut -6173",
            "deal with increment 44",
            "cut -2173",
            "deal with increment 55",
            "deal into new stack",
            "cut 5324",
            "deal with increment 58",
            "cut 592",
            "deal with increment 17",
            "cut 8744",
            "deal with increment 10",
            "cut -5679",
            "deal into new stack",
            "deal with increment 37",
            "cut 1348",
            "deal with increment 30",
            "cut -8824",
            "deal with increment 54",
            "deal into new stack",
            "cut -1263",
            "deal with increment 29",
            "deal into new stack",
            "deal with increment 13",
            "cut -9682",
            "deal with increment 19",
            "cut 8665",
            "deal with increment 42",
            "cut 3509",
            "deal with increment 57",
            "cut 7536",
            "deal with increment 42",
            "cut -1391",
            "deal with increment 25",
            "deal into new stack",
            "deal with increment 49",
            "cut 7942",
            "deal with increment 49",
            "cut -9595",
            "deal with increment 59",
            "cut 9964",
            "deal with increment 22",
            "deal into new stack",
            "cut 5436",
            "deal into new stack",
            "cut 4605",
            "deal into new stack",
            "deal with increment 36",
            "cut -2667",
            "deal with increment 49",
            "cut 4727",
            "deal into new stack",
            "cut 2236",
            "deal with increment 66",
            "cut 8098",
            "deal into new stack",
            "deal with increment 62",
            "deal into new stack",
            "deal with increment 70",
            "cut -9110"
          ]