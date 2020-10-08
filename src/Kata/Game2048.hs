module Kata.Game2048 where

import Data.List
import Data.Char (digitToInt)
import Debug.Trace (trace)
import qualified Kata.Utils as U


type Row = [Int]
type Board = [Row]
data Movement =
  Left | Right | Up | Down
  deriving (Show)


-- Movement

moveBoard :: Board -> Movement -> Board
moveBoard board mov = map (`moveRow` mov) board

moveRow :: Row -> Movement -> Row
moveRow row m = U.rightPadZero 4 $ aggregateRow $ filter (/=0) row

-- foldl (flip $ U.trace2 aggregateRow) [] row

aggregateRowFold :: Int -> [Int] -> [Int]
aggregateRowFold 0 rowRest = rowRest
aggregateRowFold el [] = [el]
aggregateRowFold el rowRest@(x:xs) = if el == x then xs ++ [2*el]
                                            else rowRest ++ [el]

aggregateRow :: [Int] -> [Int]
aggregateRow [] = []
aggregateRow [x] = [x]
aggregateRow (x:y:others) = if x == y then x+y : aggregateRow others
                                      else x   : aggregateRow (y : others)




-- IO

parseBoard :: [String] -> Board
parseBoard = map parseRow

parseRow :: String -> Row
parseRow ls = map (\num -> read num :: Int) (words ls)

parseMovement :: String -> Movement
parseMovement movLine = case digitToInt (head movLine) of
  0 -> Kata.Game2048.Left
  1 -> Kata.Game2048.Right
  2 -> Kata.Game2048.Up
  3 -> Kata.Game2048.Down
  _ -> error "Incorrect movement"

printBoard :: Board -> String
printBoard board = intercalate "\n" $ map printRow board

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

