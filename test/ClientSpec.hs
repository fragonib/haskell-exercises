module ClientSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Client

spec :: Spec
spec = do

    it "when no clients then zero gender histogram " $
      let actual = genderHistogram []
      in actual `shouldBe` GenderHistogram 0 0

    it "when client hass no individuals then zero gender histogram" $
      let actual = genderHistogram [ GovOrg "Generalitat catalana",
                                     Company "ACME" 44864646 (Person "Fran" "Gonzalez" Male) "Boss" ]
      in actual `shouldBe` GenderHistogram 0 0

    it "when mixed clients then generic gender histogram" $
      let actual = genderHistogram [ Individual (Person "Fran" "Gonzalez" Male) True,
                                     Individual (Person "Alicia" "Gonzalez" Female) True,
                                     Individual (Person "Luis" "Gil" Unknown) True,
                                     GovOrg "Generalitat catalana" ]
      in actual `shouldBe` GenderHistogram 1 1
