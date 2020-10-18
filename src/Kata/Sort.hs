{-# LANGUAGE ScopedTypeVariables #-}
module Kata.Sort where

import Data.Maybe (fromJust)
import Data.List (sortBy)
import qualified Data.Map as M


type IntMap = M.Map Int Int


-- Sort

sort :: [Int] -> [Int]
sort xs = sortBy (sortComparison fm pm) xs
  where fm = frequencyMap xs
        pm = firstPositionMap xs

sortComparison :: IntMap -> IntMap -> Int -> Int -> Ordering
sortComparison frequencyMap firstPositionMap a b =
  case frequencyComp frequencyMap a b of
    EQ -> positionComp firstPositionMap a b
    h -> h

frequencyComp :: IntMap -> Int -> Int -> Ordering
frequencyComp frequencies x y = compare freq2 freq1
  where freq1 = fromJust $ M.lookup x frequencies
        freq2 = fromJust $ M.lookup y frequencies

frequencyMap :: [Int] -> IntMap
frequencyMap xs = M.fromListWith (+) $ zip xs (repeat 1)

positionComp :: IntMap -> Int -> Int -> Ordering
positionComp positions x y = compare pos1 pos2
  where pos1 = fromJust $ M.lookup x positions
        pos2 = fromJust $ M.lookup y positions

firstPositionMap :: [Int] -> IntMap
firstPositionMap xs = M.fromListWith (curry snd) $ zip xs [1..]


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