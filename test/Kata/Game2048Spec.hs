module Kata.Game2048Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.Game2048 as SUT


spec :: Spec
spec = do

  describe "2048 movement" $ do

    it "Sample 1" $ do
      SUT.game2048CLI  "2 0 0 2"
                       "4 16 8 2"
                       "2 64 32 4"
                       "1024 1024 64 0"
                       "0"
      `shouldBe` "4 0 0 0\n\
                 \4 16 8 2\n\
                 \2 64 32 4\n\
                 \2048 64 0 0"

    it "Sample 2" $ do
      SUT.game2048CLI "2 0 0 2"
                      "4 16 8 2"
                      "2 64 32 4"
                      "1024 1024 64 0"
                      "1"
      `shouldBe` "2 16 8 4\n\
                 \4 64 32 4\n\
                 \2 1024 64 0\n\
                 \1024 0 0 0"

    it "Sample 3" $ do
      SUT.game2048CLI "2 0 0 2"
                      "4 16 8 2"
                      "2 64 32 4"
                      "1024 1024 64 0"
                      "2"
      `shouldBe` "0 0 0 4\n\
                 \4 16 8 2\n\
                 \2 64 32 4\n\
                 \0 0 2048 64"

    it "Sample 4" $ do
      SUT.game2048CLI "2 0 0 2"
                      "4 16 8 2"
                      "2 64 32 4"
                      "1024 1024 64 0"
                      "3"
      `shouldBe` "2 0 0 0\n\
                 \4 16 8 0\n\
                 \2 64 32 4\n\
                 \1024 1024 64 4"

    it "Sample 5" $ do
      SUT.game2048CLI "2 2 4 8"
                      "4 0 4 4"
                      "16 16 16 16"
                      "32 16 16 32"
                      "0"
      `shouldBe` "4 4 8 0\n\
                 \8 4 0 0\n\
                 \32 32 0 0\n\
                 \32 32 32 0"

    it "Sample 6" $ do
      SUT.game2048CLI "2 2 4 8"
                      "4 0 4 4"
                      "16 16 16 16"
                      "32 16 16 32"
                      "2"
      `shouldBe` "0 4 4 8\n\
                 \0 0 4 8\n\
                 \0 0 32 32\n\
                 \0 32 32 32"

