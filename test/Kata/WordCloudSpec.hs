module Kata.WordCloudSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.WordCloud as SUT


spec :: Spec
spec = do

  describe "word sizes" $ do

    it "word width in points" $ do
      SUT.wordWidthInPts "words" 20 `shouldBe` 57

  describe "Calculate WordCloud height" $ do

    it "sample 1" $ do

      SUT.calculateCloudHeight 260 [
        ("apple", 10),
        ("banana", 5),
        ("grape", 20),
        ("kiwi", 18),
        ("orange", 12),
        ("strawberry", 10)]
      `shouldBe` 114

    it "sample 2" $ do

      SUT.calculateCloudHeight 250 [
        ("apple", 10),
        ("banana", 5),
        ("grape", 20),
        ("kiwi", 18),
        ("orange", 12),
        ("strawberry", 10)]
      `shouldBe` 99

    it "sample 3" $ do

      SUT.calculateCloudHeight 610 [
        ("apple", 10),
        ("banana", 5),
        ("grape", 20),
        ("kiwi", 18),
        ("orange", 12),
        ("strawberry", 10)]
      `shouldBe` 48


  describe "IO" $ do

    it "sample 1" $ do

      SUT.wordCloudsCLI [
        "260 6",
        "apple 10",
        "banana 5",
        "grape 20",
        "kiwi 18",
        "orange 12",
        "strawberry 10",
        "250 6",
        "apple 10",
        "banana 5",
        "grape 20",
        "kiwi 18",
        "orange 12",
        "strawberry 10",
        "610 6",
        "apple 10",
        "banana 5",
        "grape 20",
        "kiwi 18",
        "orange 12",
        "strawberry 10",
        "0 0"
        ]
      `shouldBe` [
        "CLOUD 1: 114",
        "CLOUD 2: 99",
        "CLOUD 3: 48"
        ]
