module Kata.SimonSaysSpec where

import Test.Hspec
import qualified Kata.SimonSays as SUT


spec :: Spec
spec = do

  describe "Simon says" $ do

    it "is simon order" $ do
       SUT.isSimonOrder ["simon", "says", "write", "a", "program"] `shouldBe` True
       SUT.isSimonOrder ["print", "some", "output"] `shouldBe` False
       SUT.isSimonOrder ["simon", "whispers", "do", "not", "stress"] `shouldBe` False
       SUT.isSimonOrder ["simon", "says", "get", "balloons"] `shouldBe` True

    it "Simon CLI" $ do

      SUT.simonCLI [
        "simon says write a program",
        "print some output",
        "simon whispers do not stress",
        "simon says get balloons"]
      `shouldBe` [ 
        "write a program",
        "",
        "",
        "get balloons"]