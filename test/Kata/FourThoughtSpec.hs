module Kata.FourThoughtSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.FourThought as SUT


spec :: Spec
spec = do

  describe "four" $ do

    it "opers and precedence" $ do
      SUT.calculate [(+ 4), flip (-) 4 , (* 4)] `shouldBe` 16
      SUT.calculate [flip div 4, flip (-) 4 , (* 4)] `shouldBe` -12


  describe "find solution" $ do

    it "opers and precedence" $ do
      SUT.solutionLiteral (9, SUT.findSolution 9)  `shouldBe` "4 + 4 + 4 / 4 = 9"
      SUT.solutionLiteral (0, SUT.findSolution 0)  `shouldBe` "4 * 4 - 4 * 4 = 0"
      SUT.solutionLiteral (7, SUT.findSolution 7)  `shouldBe` "4 + 4 - 4 / 4 = 7"
      SUT.solutionLiteral (11, SUT.findSolution 11) `shouldBe` "no solution"
      SUT.solutionLiteral (24, SUT.findSolution 24) `shouldBe` "4 * 4 + 4 + 4 = 24"


