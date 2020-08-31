module Chapter3.HighOrderFunctionsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Chapter3.HighOrderFunctions as SUT
import Chapter2.Client

spec :: Spec
spec = do

    it "filterOnes" $
      SUT.filterOnes [1, 2, 3, 1, 2] `shouldBe` [1, 1]

    it "filterANumber" $
      SUT.filterANumber 2 [1, 2, 3, 1, 2] `shouldBe` [2, 2]

    it "filterNot" $
      SUT.filterNot (> 2) [1, 2, 3, 1, 2] `shouldBe` [1, 2, 1, 2]

    it "filterGovOrgs" $
      let actual = [ Individual (Person "Fran" "Gonzalez" Male) True,
                     Company "ACME" 44864646 (Person "Fran" "Gonzalez" Male) "Boss",
                     GovOrg "Generalitat catalana" ]
      in SUT.filterGovOrgs actual `shouldBe` [ GovOrg "Generalitat catalana" ]