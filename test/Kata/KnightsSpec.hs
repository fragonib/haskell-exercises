module Kata.KnightsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.Knights as SUT


spec :: Spec
spec = do

  describe "knight movements" $ do

    it "when top-left position" $ do
      SUT.knightMovements (0, 0) `shouldBe` [(1,2), (2,1)]
      
    it "when down-rigth position" $ do
      SUT.knightMovements (4, 4) `shouldBe` [(2,3), (3,2)]
      
    it "when middle position" $ do
      SUT.knightMovements (2, 2) `shouldBe` [(0,1), (0,3), (1,0), (1,4), (3,0), (3,4), (4,1), (4,3)]


  describe "valid knight board" $ do
  
    -- ...k.
    -- ...k.
    -- k.k..
    -- .k.k.
    -- k.k.k
    it "board sample 1" $
      SUT.isNineKnightBoard [(0,3), (1,3), (2,0), (2,2), (3,1), (3,3), (4,0), (4,2), (4,4)] `shouldBe` False

    -- .....
    -- ...k.
    -- k.k.k
    -- .k.k.
    -- k.k.k
    it "board sample 2" $
      SUT.isNineKnightBoard [(1,3), (2,0), (2,2), (2,4), (3,1), (3,3), (4,0), (4,2), (4,4)] `shouldBe` True

    -- .....
    -- ...k.
    -- k.k.k
    -- .k.k.
    -- k...k
    it "board sample 3" $
      SUT.isNineKnightBoard [(1,3), (2,0), (2,2), (2,4), (3,1), (3,3), (4,0), (4,4)] `shouldBe` False
