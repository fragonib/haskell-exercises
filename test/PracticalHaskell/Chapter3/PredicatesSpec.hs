module PracticalHaskell.Chapter3.PredicatesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified PracticalHaskell.Chapter3.Predicates as SUT
import PracticalHaskell.Chapter3.ClientRecords

spec :: Spec
spec = do

  describe "elem" $ do

    it "present" $ do
      3 `elem` [1, 2, 3, 4, 5] `shouldBe` True

    it "not present" $ do
      6 `elem` [1, 2, 3, 4, 5] `shouldBe` False

  describe "company duties analytics" $ do

    it "all" $
      let actual = SUT.companyDutiesAnalyticsPF
              [ Individual { person = Person "Fran" "Gonzalez" Male, boss = True },
                Company "ACME" 11111111 (Person "Fran" "Gonzalez" Male) "CTO",
                Company "POOL" 22222222 (Person "Luis" "Gil" Male) "CEO",
                Company "AQQA" 33333333 (Person "Daniel" "Mateo" Male) "CEO",
                GovOrg "Generalitat catalana" ]
      in actual `shouldBe` ["20%", "35%"]