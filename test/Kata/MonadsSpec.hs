{-# LANGUAGE FlexibleInstances #-}

module Kata.MonadsSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad.Writer 
import Control.Monad.State
import qualified Kata.Monads as SUT


spec :: Spec
spec = do

  describe "addStuff monad equivalents" $ do

    it "addMaybe" $ do
       -- Expectation: Just 8 = Just (2 + (2*3))
       SUT.addMaybe (Just 2) (\x -> Just (x*3)) 
          `shouldBe` SUT.add (Just 2) (\x -> Just (x*3))

    it "addList" $ do
       -- Expectation: [11,101,22,202,33,303] = [1+10, 1+100, 2+20, 2+200, 3+30, 3+300]
       SUT.addList [1, 2, 3] (\x -> [x*10, x*100])
          `shouldBe` SUT.add [1, 2, 3] (\x -> [x*10, x*100])

    it "addWriter" $ do
       -- Expectation: IntWriterM (8, [Input: 2", "Oper: 6"]) = IntWriterM
       SUT.addWriter (SUT.intWriter 2 ["Input: 2"])
                     (\x -> SUT.intWriter (x*3) ["Oper: " ++ show (x*3)])
          `shouldBe` SUT.add (SUT.intWriter 2 ["Input: 2"])
                                          (\x -> SUT.intWriter (x*3) ["Oper: " ++ show (x*3)])

    it "addReader" $ do
       -- Expectation: 20 = (3+2) + (3+2)*3
       SUT.addReader (+2) (*) 3
          `shouldBe` SUT.add (+2) (*) 3

    it "addState" $ do
       -- Expectation: IntStateM (8, "State 0") = IntStateM (2+(2*3), "State 0")
       runState 
          (SUT.addState (state (\ s -> (2, s))) (\ value -> state (\ s -> (value * 3, s)))) "State 0"
       `shouldBe` runState 
          (SUT.add (state (\s -> (2, s))) (\value -> state (\s -> (value*3, s)))) "State 0"

