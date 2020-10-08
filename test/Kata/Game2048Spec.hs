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