module Kata.SnapperSpec where

import Test.Hspec
import Kata.Snapper (isLightOn, Snapper(..), snapperCLI)


spec :: Spec
spec = do

  describe "Is ligth on?" $ do

    it "1 0 -> False" $ do
       isLightOn 1 0 `shouldBe` OFF
          
    it "1 1 -> True" $ do
       isLightOn 1 1 `shouldBe` ON    
      
    it "4 0 -> False" $ do
       isLightOn 4 0 `shouldBe` OFF
          
    it "4 47 -> True" $ do
       isLightOn 4 47 `shouldBe` ON


  describe "IO" $ do

    it "sample 1" $ do

       snapperCLI [
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
        
        
