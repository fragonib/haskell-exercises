module Kata.Knights where

import Data.List
import Debug.Trace (trace)

tableSize = 5

isNineKnightBoard :: [(Int, Int)] -> Bool
isNineKnightBoard knights
  | length knights /= 9 = False
  | otherwise = isBoardCaptureFree knights

isBoardCaptureFree :: [(Int, Int)] -> Bool
isBoardCaptureFree [knight] = True
isBoardCaptureFree (knight:knights) = null (knightMovements knight `intersect` knights) &&
                                      isBoardCaptureFree knights

knightMovements :: (Int, Int) -> [(Int, Int)]
knightMovements (a, b)
  | a >= tableSize || b >= tableSize = error "Invalid position"
  | otherwise = filter insideBoardPosition [
                          (a-2, b-1),
                          (a-2, b+1),
                          (a-1, b-2),
                          (a-1, b+2),
                          (a+1, b-2),
                          (a+1, b+2),
                          (a+2, b-1),
                          (a+2, b+1)]

insideBoardPosition :: (Int, Int) -> Bool
insideBoardPosition (a, b)
  | a >= tableSize = False
  | b >= tableSize = False
  | a < 0 = False
  | b < 0 = False
  | otherwise = True


main :: IO()
main = print $ isNineKnightBoard [(2,2)]
