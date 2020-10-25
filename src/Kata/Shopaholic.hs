{-# LANGUAGE ScopedTypeVariables #-}
module Kata.Shopaholic where

import Data.List


totalDiscount :: [Int] -> Int
totalDiscount xs = foldr ((+) . last) 0 byThree
  where sorted = reverse $ sort xs
        byThree = groupByElementsNumber 3 sorted

groupByElementsNumber :: Int -> [a] -> [[a]]
groupByElementsNumber _ [] = []
groupByElementsNumber n l
  | n > 0 = take n l : groupByElementsNumber n (drop n l)
  | otherwise = error "Negative or zero n"


main :: IO()
main = do
  inputLines <- lines <$> getContents
  let prices::[Int] = map read $ words (inputLines !! 1)
  print $ totalDiscount prices
