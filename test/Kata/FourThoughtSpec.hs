module Kata.FourThoughtSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.FourThought as SUT


spec :: Spec
spec = do

  describe "four" $ do

    it "opers and precedence" $ do
      SUT.calculateSolution [(+ 4), flip (-) 4 , (* 4)] `shouldBe` 16
      SUT.calculateSolution [flip div 4, flip (-) 4 , (* 4)] `shouldBe` -12


  describe "find solution" $ do

    it "sample 1" $ do
      SUT.solutionLiteral (9, SUT.findSolution 9)  `shouldBe` "4 + 4 + 4 / 4 = 9"

    it "sample 2" $ do
      SUT.solutionLiteral (0, SUT.findSolution 0)  `shouldBe` "4 * 4 - 4 * 4 = 0"

    it "sample 3" $ do
      SUT.solutionLiteral (7, SUT.findSolution 7)  `shouldBe` "4 + 4 - 4 / 4 = 7"

    it "sample 4" $ do
      SUT.solutionLiteral (11, SUT.findSolution 11) `shouldBe` "no solution"

    it "sample 5" $ do
      SUT.solutionLiteral (24, SUT.findSolution 24) `shouldBe` "4 * 4 + 4 + 4 = 24"


