{-# LANGUAGE ScopedTypeVariables #-}

module Kata.Sort where

import Data.List (intercalate, groupBy, group, sortBy)
import qualified Data.Map as M
import Data.Maybe (fromJust)


type FreqMap = M.Map Int Int


-- Sort

sort :: [Int] -> [Int]
sort xs = sortBy (frequencyComp $ histogram xs) xs

histogram :: [Int] -> FreqMap
histogram xs = M.fromListWith (+) $ zip xs (repeat 1)

--histogram2 :: [Int] -> [(Int, Int)]
--histogram2 xs = map (\g -> (head g, length g)) $ groupBy (==) xs

frequencyComp :: FreqMap -> Int -> Int -> Ordering
frequencyComp histo x y = compare freq2 freq1
  where freq1 = fromJust $ M.lookup x histo
        freq2 = fromJust $ M.lookup y histo


-- IO

main :: IO()
main = do
    firstLine <- getLine
    secondLine <- getLine
    let indexes::[Int] = map read $ words firstLine
        count = head indexes
        max = head $ tail indexes
        numbers = map read $ words secondLine
    putStrLn $ unwords $ map show $ sort numbers