module Kata.MonadsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad.Writer 
import qualified Kata.Monads as SUT


spec :: Spec
spec = do

  describe "addStuff monad equivalents" $ do

    it "addMaybe" $ do
       -- Just 8 = Just 2 + (2*3)
       SUT.addMaybe (Just 2) (\x -> Just (x*3)) 
          `shouldBe` SUT.add (Just 2) (\x -> Just (x*3)) 

    it "addList" $ do
       -- [11,101,22,202,33,303] = [1+10, 1+100, 2+20, 2+200, 3+30, 3+300]
       SUT.addList [1, 2, 3] (\x -> [x*10, x*100]) 
          `shouldBe` SUT.add [1, 2, 3] (\x -> [x*10, x*100])
           
    it "addWriter" $ do
       -- IntWriterM (8, "Input: 2\nOper: 6") = IntWriterM
       SUT.addWriter (SUT.intWriter 2 "Input: 2\n") 
                     (\x -> SUT.intWriter (x*3) ("Oper: " ++ show (x*3)))
          `shouldBe` SUT.add (SUT.intWriter 2 "Input: 2\n") 
                     (\x -> SUT.intWriter (x*3) ("Oper: " ++ show (x*3)))
          
    it "addReader" $ do
       -- 20 = (3+2) + (3+2)*3
       SUT.addReader (+2) (*) 3
          `shouldBe` SUT.add (+2) (*) 3 
    
        