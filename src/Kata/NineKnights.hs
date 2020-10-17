module Kata.NineKnights where

import Data.List
import Kata.Utils (enumerate, (.||.))
import Debug.Trace (trace)


boardSize = 5

type Knight = (Int, Int)
type Board = [Knight]



-- Validating input

isValidSymbol :: Char -> Bool
isValidSymbol = (== '.') .||. (== 'k')

isValidBoardLiteral :: [Char] -> Bool
isValidBoardLiteral board | length board /= (boardSize * boardSize) = False
isValidBoardLiteral board = foldr ((&&) . isValidSymbol) True board


-- Parsing input

symbolsToBoard :: [Char] -> [Knight]
symbolsToBoard = foldr symbolToKnight [] . enumerate

symbolToKnight :: (Int, Char) -> [Knight] -> [Knight]
symbolToKnight (index, symbol) knights = case symbol of
  'k' -> indexToPosition index : knights
  _ -> knights


indexToPosition :: Int -> (Int, Int)
indexToPosition index = index `divMod` boardSize


-- API

isNineKnightBoard :: Board -> Bool
isNineKnightBoard knights
  | length knights /= 9 = False
  | otherwise = isCaptureFreeBoard knights

isCaptureFreeBoard :: Board -> Bool
isCaptureFreeBoard [] = True
isCaptureFreeBoard [single] = True
isCaptureFreeBoard (knight:others) = isCapturingOtherKnights knight others &&
                                     isCaptureFreeBoard others

isCapturingOtherKnights :: Knight -> [Knight] -> Bool
isCapturingOtherKnights knight others = null (knightMovements knight `intersect` others)

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

validationToLiteral :: Bool -> [Char]
validationToLiteral b = if b then "valid" else "invalid"

isNineKnightBoardCLI :: [Char] -> [Char]
isNineKnightBoardCLI = validationToLiteral . isNineKnightBoard . symbolsToBoard


main :: IO()
main = do
    l1 <- getLine
    l2 <- getLine
    l3 <- getLine
    l4 <- getLine
    l5 <- getLine
    putStrLn $ let input = (l1 ++ l2 ++ l3 ++ l4 ++ l5)
               in isNineKnightBoardCLI input