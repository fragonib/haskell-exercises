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


boolToValidation :: Bool -> [Char]
boolToValidation b = if b then "valid" else "invalid"

isNineKnightBoardIO :: [Char] -> [Char]
isNineKnightBoardIO = boolToValidation . isNineKnightBoard . symbolsToBoard

main :: IO()
main = do
    l1 <- getLine
    l2 <- getLine
    l3 <- getLine
    l4 <- getLine
    l5 <- getLine
    putStrLn $ let input = (l1 ++ l2 ++ l3 ++ l4 ++ l5)
               in isNineKnightBoardIO input

    -- foldr (\i text -> text ++ readLn) [] [1..boardSize]
