module FizzBuzzKata.FizzBuzz where

import Data.List

fizzbuzz :: Int -> [Char]
fizzbuzz x
  | (x `mod` 3 == 0) && (x `mod` 5 == 0) = "FizzBuzz"
  | x `mod` 3 == 0 = "Fizz"
  | x `mod` 5 == 0 = "Buzz"
  | otherwise = show x


topFizzBuzz :: Int -> [[Char]]
topFizzBuzz n = map fizzbuzz [1..n]

topFizzBuzzAsUnfold :: Int -> [[Char]]
topFizzBuzzAsUnfold n = unfoldr (\x -> if x > n then Nothing else Just (fizzbuzz x, x+1)) 1

main :: IO()
main = print $ topFizzBuzzAsUnfold 100
