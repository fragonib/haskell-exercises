module Kata.FizzBuzzSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Kata.FizzBuzz as SUT


spec :: Spec
spec = do

  describe "fizzbuzz" $ do

    it "several lists fizzbuzz are rule compliant" $ 
      property fizzBuzzProperty

    it "top 100 list is equal to top 100 golden master" $
      SUT.topFizzBuzzAsUnfold 100 `shouldBe` goldenMaster
      where goldenMaster = ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz",
                            "11","Fizz","13","14","FizzBuzz","16","17","Fizz","19","Buzz",
                            "Fizz","22","23","Fizz","Buzz","26","Fizz","28","29","FizzBuzz",
                            "31","32","Fizz","34","Buzz","Fizz","37","38","Fizz","Buzz",
                            "41","Fizz","43","44","FizzBuzz","46","47","Fizz","49","Buzz",
                            "Fizz","52","53","Fizz","Buzz","56","Fizz","58","59","FizzBuzz",
                            "61","62","Fizz","64","Buzz","Fizz","67","68","Fizz","Buzz",
                            "71","Fizz","73","74","FizzBuzz","76","77","Fizz","79","Buzz",
                            "Fizz","82","83","Fizz","Buzz","86","Fizz","88","89","FizzBuzz",
                            "91","92","Fizz","94","Buzz","Fizz","97","98","Fizz","Buzz" ]


fizzBuzzProperty :: Positive Int -> Property
fizzBuzzProperty n = forAll (elements $ zip [1..] (SUT.topFizzBuzzAsUnfold (getPositive n))) validFizzBuzzItem
                      where validFizzBuzzItem (index, text) =
                              case text of
                                "Fizz" -> index `mod` 3 == 0
                                "Buzz" -> index `mod` 5 == 0
                                "FizzBuzz" -> (index `mod` 3 == 0) && (index `mod` 5 == 0)
                                _ -> show index == text