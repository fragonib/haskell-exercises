module Kata.Knights where

import Data.List
import Debug.Trace (trace)


boardSize = 5

type Knight = (Int, Int)
type Board = [Knight]

or' :: (a -> Bool) -> (a -> Bool) -> a -> Bool
or' f g a = f a || g a

enumerate :: (Num a, Enum a) => [b] -> [(a, b)]
enumerate = zip [0..]

isValidSymbol :: Char -> Bool
isValidSymbol = (== '.') `or'` (== 'k')

isValidBoardLiteral :: [Char] -> Bool
isValidBoardLiteral board | length board /= (boardSize * boardSize) = False
isValidBoardLiteral board = foldr ((&&) . isValidSymbol) True board

symbolsToBoard :: [Char] -> [Knight]
symbolsToBoard = foldr symbolToKnight [] . enumerate

symbolToKnight :: (Int, Char) -> [Knight] -> [Knight]
symbolToKnight (index, symbol) knights = case symbol of
  'k' -> index `divMod` boardSize : knights
  _ -> knights

isNineKnightBoard :: Board -> Bool
isNineKnightBoard knights
  | length knights /= 9 = False
  | otherwise = isCaptureFreeBoard knights

isCaptureFreeBoard :: Board -> Bool
isCaptureFreeBoard [] = True
isCaptureFreeBoard [knight] = True
isCaptureFreeBoard (first:others) = null (knightMovements first `intersect` others)
                                    && isCaptureFreeBoard others

knightMovements :: Knight -> [Knight]
knightMovements knight@(x, y)
  | not $ isInsideBoard knight = error "Invalid position"
  | otherwise = filter isInsideBoard [(x-2, y-1), (x-2, y+1),
                                      (x-1, y-2), (x-1, y+2),
                                      (x+1, y-2), (x+1, y+2),
                                      (x+2, y-1), (x+2, y+1)]

isInsideBoard :: Knight -> Bool
isInsideBoard (a, b)
  | a < 0 || a >= boardSize = False
  | b < 0 || b >= boardSize = False
  | otherwise = True


main :: IO()
main = print $ isNineKnightBoard []
