module PracticalHaskell.Chapter3.FoldsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import PracticalHaskell.Chapter3.ClientRecords
import qualified PracticalHaskell.Chapter3.Folds as SUT

spec :: Spec
spec = do

  describe "With pattern matching" $ do

    it "product of a integer list" $
      let actual = SUT.productPM [1, 2, 3, 4, 5]
      in actual `shouldBe` 120

    it "client with minimum name length" $
      let actual = SUT.minimumClientPM [ Individual (Person "Fran" "Gonzalez" Male) True,
                                     Individual (Person "Alicia" "Gonzalez" Female) True,
                                     Individual (Person "Luis" "Gil" Unknown) True,
                                     Company "ACME" 44864646 (Person "Fran" "Gonzalez" Male) "Boss",
                                     GovOrg "Generalitat catalana" ]
      in actual `shouldBe` Company "ACME" 44864646 (Person "Fran" "Gonzalez" Male) "Boss"

    it "conjuntion of all conditions" $ do
      SUT.allPM [True] `shouldBe` True
      SUT.allPM [False] `shouldBe` False
      SUT.allPM [True, False, True] `shouldBe` False
      SUT.allPM [True, True, True] `shouldBe` True

  describe "With FOLD" $ do

    it "product of a integer list" $
      let actual = SUT.productFold [1, 2, 3, 4, 5]
      in actual `shouldBe` 120

    it "client with minimum name length" $
      let actual = SUT.minimumClientFold [ Individual (Person "Fran" "Gonzalez" Male) True,
                                     Individual (Person "Alicia" "Gonzalez" Female) True,
                                     Individual (Person "Luis" "Gil" Unknown) True,
                                     Company "ACME" 44864646 (Person "Fran" "Gonzalez" Male) "Boss",
                                     GovOrg "Generalitat catalana" ]
      in actual `shouldBe` Company "ACME" 44864646 (Person "Fran" "Gonzalez" Male) "Boss"

    it "conjuntion of all conditions" $ do
      SUT.allFold [True] `shouldBe` True
      SUT.allFold [False] `shouldBe` False
      SUT.allFold [True, False, True] `shouldBe` False
      SUT.allFold [True, True, True] `shouldBe` True

    it "minimum by" $ do
      SUT.minimumBy (\x -> -x) [1, 2, 3] `shouldBe` 3
