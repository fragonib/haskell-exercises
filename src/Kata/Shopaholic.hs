module Kata.Shopaholic where

import Data.List


-- Constants

itemsWithDiscount :: Int
itemsWithDiscount = 3


-- Core

totalDiscount :: [Int] -> Int
totalDiscount xs = foldr ((+) . last) 0 onlyThree
  where sorted = reverse $ sort xs
        byThree = groupByElementsNumber itemsWithDiscount sorted
        onlyThree = filter ((== itemsWithDiscount) . length) byThree

groupByElementsNumber :: Int -> [a] -> [[a]]
groupByElementsNumber _ [] = []
groupByElementsNumber n l
  | n > 0 = take n l : groupByElementsNumber n (drop n l)
  | otherwise = error "Negative or zero n"


-- IO

main :: IO()
main = do
  inputLines <- lines <$> getContents
  let prices = map read $ words (inputLines !! 1)
  print $ totalDiscount prices
