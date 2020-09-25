module Kata.Knights where

import Data.List

tableSize = 5

knightMovement :: (Int, Int) -> [(Int, Int)]
knightMovement (a, b)
  | a >= tableSize || b >= tableSize = error "Invalid position"
  | otherwise = filter insideBoard [ 
                          (a-2, b-1),
                          (a-2, b+1),
                          (a-1, b-2),
                          (a-1, b+2),
                          (a+1, b-2),
                          (a+1, b+2),
                          (a+2, b-1),
                          (a+2, b+1)
                       ]

insideBoard :: (Int, Int) -> Bool
insideBoard (a, b)
  | a >= tableSize = False
  | b >= tableSize = False
  | a < 0 = False
  | b < 0 = False
  | otherwise = True

main :: IO()
main = print $ knightMovement (2,2)
