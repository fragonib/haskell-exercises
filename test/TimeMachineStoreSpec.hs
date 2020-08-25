module TimeMachineStoreSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import TimeMachineStore
import Client

spec :: Spec
spec = do

    it "zero histogram when input is empty" $
      let actual = genderHistogram []
      in actual `shouldBe` GenderHistogram 0 0

    it "zero histogram when input has no individuals" $
      let actual = genderHistogram [ GovOrg "Generalitat catalana",
                                     Company "ACME" 44864646 (Person "Fran" "Gonzalez" Male) "Boss" ]
      in actual `shouldBe` GenderHistogram 0 0

    it "generic histogram when mixed clients" $
      let actual = genderHistogram [ Individual (Person "Fran" "Gonzalez" Male) True,
                                     Individual (Person "Alicia" "Gonzalez" Female) True,
                                     Individual (Person "Luis" "Gil" Unknown) True,
                                     GovOrg "Generalitat catalana" ]
      in actual `shouldBe` GenderHistogram 1 1
