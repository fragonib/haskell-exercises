module Kata.SnapperSpec where

import Test.Hspec
import qualified Kata.Snapper as SUT


spec :: Spec
spec = do

  describe "Is ligth on?" $ do

    it "1 0 -> False" $ do
      SUT.isLightOn 1 0 `shouldBe` False
          
    it "1 1 -> True" $ do
      SUT.isLightOn 1 1 `shouldBe` True    
      
    it "4 0 -> False" $ do
      SUT.isLightOn 4 0 `shouldBe` False
          
    it "4 47 -> True" $ do
      SUT.isLightOn 4 47 `shouldBe` True


  describe "IO" $ do

    it "sample 1" $ do

      SUT.snapperCLI [
        "1 0",
        "1 1",
        "4 0",
        "4 47"
        ]
      `shouldBe` [
        "Case #1: OFF",
        "Case #2: ON",
        "Case #3: OFF",
        "Case #4: ON"
        ]
        
        
