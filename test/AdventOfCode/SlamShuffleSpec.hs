module AdventOfCode.SlamShuffleSpec where

import Test.Hspec
import AdventOfCode.SlamShuffle

-- Specs

spec :: Spec
spec = do

  describe "Permutations" $ do

    it "by restacking" $ do
      let finalPerm = shuffleByRestacking 10
       in map (runPerm finalPerm) [0..9] `shouldBe` [9,8,7,6,5,4,3,2,1,0]

    it "by cutting positive" $ do
      let finalPerm = shuffleByCutting 10 3
       in map (runPerm finalPerm) [0..9] `shouldBe` [7,8,9,0,1,2,3,4,5,6]

    it "by cutting negative" $ do
      let finalPerm = shuffleByCutting 10 (-4)
       in map (runPerm finalPerm) [0..9] `shouldBe` [4,5,6,7,8,9,0,1,2,3]

    it "with increment" $ do
      let finalPerm = shuffleWithIncrement 10 3
       in map (runPerm finalPerm) [0..9] `shouldBe` [0,3,6,9,2,5,8,1,4,7]


  describe "Small deck" $ do

    it "mixing shuffles 1" $ do
      -- Start: 0 1 2 3 4 5 6 7 8 9
      -- End:   0 3 6 9 2 5 8 1 4 7
      let finalPerm = slamShuffle 10 [
                    "deal with increment 7",
                    "deal into new stack",
                    "deal into new stack"
                 ]
       in map (runPerm finalPerm) [0..9] `shouldBe` [0,7,4,1,8,5,2,9,6,3]

    it "mixing shuffles 2" $ do
      -- Start: 0 1 2 3 4 5 6 7 8 9
      -- Cut 6: 6 7 8 9 0 1 2 3 4 5
      -- Inc 7: 6 9 2 5 8 1 4 7 0 3
      -- End:   3 0 7 4 1 8 5 2 9 6
      let finalPerm = slamShuffle 10 [
                    "cut 6",
                    "deal with increment 7",
                    "deal into new stack"
                 ]
       in map (runPerm finalPerm) [0..9] `shouldBe` [1,4,7,0,3,6,9,2,5,8]

    it "mixing shuffles 3" $ do
      -- Start:  0 1 2 3 4 5 6 7 8 9
      -- Int 7:  0 3 6 9 2 5 8 1 4 7
      -- Int 9:  0 7 4 1 8 5 2 9 6 3
      -- Cut -2: 6 3 0 7 4 1 8 5 2 9
      let finalPerm = slamShuffle 10 [
                    "deal with increment 7",
                    "deal with increment 9",
                    "cut -2"
                 ]
       in map (runPerm finalPerm) [0..9] `shouldBe` [2,5,8,1,4,7,0,3,6,9]

    it "mixing shuffles 4" $ do
      -- Start: 0 1 2 3 4 5 6 7 8 9
      -- End:   9 2 5 8 1 4 7 0 3 6
      let finalPerm = slamShuffle 10 [
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
       in map (runPerm finalPerm) [0..9] `shouldBe` [7,4,1,8,5,2,9,6,3,0]


  describe "Big deck" $ do

    it "one card destination" $ do
      runPerm finalPerm 2019 `shouldBe` 7665
       where finalPerm = slamShuffle bigPileSize shuffleSequence
             bigPileSize = 10007
             shuffleSequence =
                [
                  "deal with increment 73",
                  "cut -8387",
                  "deal with increment 41",
                  "cut 190",
                  "deal with increment 4",
                  "cut 6396",
                  "deal with increment 47",
                  "cut -9579",
                  "deal with increment 47",
                  "cut -1296",
                  "deal with increment 2",
                  "cut 3807",
                  "deal with increment 75",
                  "cut 8267",
                  "deal with increment 53",
                  "cut 5108",
                  "deal with increment 20",
                  "cut -62",
                  "deal with increment 63",
                  "cut 4435",
                  "deal into new stack",
                  "deal with increment 2",
                  "cut 8436",
                  "deal with increment 52",
                  "cut 8420",
                  "deal with increment 70",
                  "cut -7602",
                  "deal with increment 39",
                  "cut 6737",
                  "deal into new stack",
                  "cut -3549",
                  "deal with increment 63",
                  "deal into new stack",
                  "cut -2925",
                  "deal with increment 59",
                  "cut -9525",
                  "deal with increment 12",
                  "deal into new stack",
                  "deal with increment 7",
                  "cut 4619",
                  "deal with increment 27",
                  "cut 7141",
                  "deal with increment 69",
                  "cut 5221",
                  "deal with increment 19",
                  "cut 4288",
                  "deal into new stack",
                  "deal with increment 64",
                  "cut -1618",
                  "deal with increment 63",
                  "cut -9384",
                  "deal with increment 24",
                  "deal into new stack",
                  "deal with increment 54",
                  "cut 429",
                  "deal into new stack",
                  "cut 2190",
                  "deal with increment 28",
                  "cut -4420",
                  "deal with increment 10",
                  "cut 6968",
                  "deal with increment 34",
                  "cut 8566",
                  "deal with increment 4",
                  "cut 8979",
                  "deal with increment 58",
                  "deal into new stack",
                  "deal with increment 17",
                  "deal into new stack",
                  "cut -3775",
                  "deal with increment 72",
                  "cut 3378",
                  "deal with increment 40",
                  "cut -7813",
                  "deal into new stack",
                  "deal with increment 26",
                  "deal into new stack",
                  "cut 5504",
                  "deal with increment 64",
                  "deal into new stack",
                  "cut 3592",
                  "deal with increment 13",
                  "cut 4123",
                  "deal into new stack",
                  "deal with increment 67",
                  "deal into new stack",
                  "cut 1943",
                  "deal with increment 72",
                  "cut -5205",
                  "deal into new stack",
                  "deal with increment 12",
                  "cut 1597",
                  "deal with increment 10",
                  "cut 4721",
                  "deal with increment 36",
                  "cut 3379",
                  "deal into new stack",
                  "cut -5708",
                  "deal with increment 61",
                  "cut 6852"
                ]