module Kata.NineKnightsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.NineKnights as SUT


spec :: Spec
spec = do

  describe "Symbols to board" $ do

    it "sample 1" $ do
      SUT.symbolsToBoard "\
          \...k.\
          \...k.\
          \k.k..\
          \.k.k.\
          \k.k.k" 
        `shouldBe` [(0,3), (1,3), (2,0), (2,2), (3,1), (3,3), (4,0), (4,2), (4,4)]
                                  
    it "sample 2" $ do
      SUT.symbolsToBoard "\
          \.....\
          \...k.\
          \k.k.k\
          \.k.k.\
          \k.k.k" 
        `shouldBe` [(1,3), (2,0), (2,2), (2,4), (3,1), (3,3), (4,0), (4,2), (4,4)]
                                            
    it "sample 2" $ do
      SUT.symbolsToBoard "\
          \.....\
          \...k.\
          \k.k.k\
          \.k.k.\
          \k...k" 
        `shouldBe` [(1,3), (2,0), (2,2), (2,4), (3,1), (3,3), (4,0), (4,4)]


  describe "Calculate knight movements" $ do

    it "from top-left position" $ do
      SUT.knightMovements (0, 0) 
      `shouldBe` [(1,2), (2,1)]
      
    it "from down-rigth position" $ do
      SUT.knightMovements (4, 4) 
      `shouldBe` [(2,3), (3,2)]
      
    it "from middle board position" $ do
      SUT.knightMovements (2, 2) 
      `shouldBe` [(0,1), (0,3), (1,0), (1,4), (3,0), (3,4), (4,1), (4,3)]


  describe "Is nine knight board" $ do
  
    {-
      ...k.
      ...k.
      k.k..
      .k.k.
      k.k.k
    -}
    it "sample 1" $
      SUT.isNineKnightBoard [(0,3), (1,3), (2,0), (2,2), (3,1), (3,3), (4,0), (4,2), (4,4)] 
      `shouldBe` False

    {-
      .....
      ...k.
      k.k.k
      .k.k.
      k.k.k
    -}
    it "sample 2" $
      SUT.isNineKnightBoard [(1,3), (2,0), (2,2), (2,4), (3,1), (3,3), (4,0), (4,2), (4,4)] 
      `shouldBe` True

    {-
      .....
      ...k.
      k.k.k
      .k.k.
      k...k
    -}
    it "sample 3" $
      SUT.isNineKnightBoard [(1,3), (2,0), (2,2), (2,4), (3,1), (3,3), (4,0), (4,4)] 
      `shouldBe` False
