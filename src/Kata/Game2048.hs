module Kata.Game2048 where

import Data.List (transpose, intercalate)
import Data.Char (digitToInt)
import qualified Kata.Utils as U


type Row = [Int]
type Board = [Row]
data Movement =
  Left | Right | Up | Down
  deriving (Show)


-- Movement

moveBoard :: Board -> Movement -> Board
moveBoard board mov =
  case mov of
    Kata.Game2048.Left -> map foldRowLeft board
    Kata.Game2048.Right -> map foldRowRight board
    Kata.Game2048.Up -> transpose $ map foldRowLeft $ transpose board
    Kata.Game2048.Down -> transpose $ map foldRowRight $ transpose board

foldRowLeft :: Row -> Row
foldRowLeft row = U.rightPadZero 4 $ aggregateRow $ filter (/=0) row

foldRowRight :: Row -> Row
foldRowRight row = reverse $ foldRowLeft $ reverse row

aggregateRow :: [Int] -> [Int]
aggregateRow [] = []
aggregateRow [x] = [x]
aggregateRow (x:y:others) = if x == y then x+y : aggregateRow others
                                      else x   : aggregateRow (y : others)

--aggregateRowFold :: Int -> [Int] -> [Int]
--aggregateRowFold 0 rowRest = rowRest
--aggregateRowFold el [] = [el]
--aggregateRowFold el rowRest@(x:xs) = if el == x then xs ++ [2*el]
--                                                else rowRest ++ [el]

-- IO

parseBoard :: [String] -> Board
parseBoard = map parseRow

parseRow :: String -> Row
parseRow ls = map (\num -> read num :: Int) (words ls)

parseMovement :: String -> Movement
parseMovement movLine = case digitToInt (head movLine) of
  0 -> Kata.Game2048.Left
  1 -> Kata.Game2048.Up
  2 -> Kata.Game2048.Right
  3 -> Kata.Game2048.Down
  _ -> error "Incorrect movement"

printBoard :: Board -> String
printBoard board = intercalate "\n" $ map printRow wboard

printRow :: Row -> String
printRow row = unwords $ map show row


-- CLI

game2048CLI :: String -> String -> String -> String -> String -> String
game2048CLI row1Line row2Line row3Line row4Line movLine =
  printBoard $ moveBoard board movement
  where board = parseBoard [row1Line, row2Line, row3Line, row4Line]
        movement = parseMovement movLine

main :: IO()
main = do
  row1Line <- getLine
  row2Line <- getLine
  row3Line <- getLine
  row4Line <- getLine
  movementLine <- getLine
  putStrLn $ game2048CLI row1Line row2Line row3Line row4Line movementLine

