module Kata.MonadsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad.Writer 
import qualified Kata.Monads as SUT


spec :: Spec
spec = do

  describe "add monad" $ do

    it "addMaybe" $ do
       SUT.addMaybe (Just 2) (\x -> Just (x*3)) 
          `shouldBe` SUT.add (Just 2) (\x -> Just (x*3))

    it "addList" $ do
       SUT.addList [1, 2, 3] (\x -> [x-1, x+1]) 
          `shouldBe` SUT.add [1, 2, 3] (\x -> [x-1, x+1])
           
    it "addWriter" $ do
       SUT.addWriter (SUT.intWriter 2 "Input: 2") (\x -> SUT.intWriter (x*3) ("Oper: " ++ show (x*3)))
          `shouldBe` SUT.add (SUT.intWriter 2 "Input: 2") (\x -> SUT.intWriter (x*3) ("Oper: " ++ show (x*3)))
          
    it "addReader" $ do
       SUT.addReader (+3) (*) 2
          `shouldBe` SUT.add (+3) (*) 2 
    
        