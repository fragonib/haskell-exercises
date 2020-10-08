module Kata.Game2048 where

import qualified Kata.Utils as U
import Data.List
import Data.Char (digitToInt)


type Row = [Int]
type Board = [Row]
data Movement = 
  Left | 
  Right | 
  Up | 
  Down
  deriving (Show)


-- Movement

game2048 :: Board -> Movement -> Board
game2048 board mov = map (`moveRow` mov) board

moveRow :: Row -> Movement -> Row
moveRow row m = 
  U.rightPadZero 4 added
    where shift = foldr (\x acum -> if x /= 0 then x:acum else acum) [] row
          added = foldr aggregateRow [] shift
        
aggregateRow :: Int -> [Int] -> [Int]      
aggregateRow e [] = [e]
aggregateRow e r@(y:ys) = if e == y then e*2:ys else e:r


-- IO

parseBoard :: [String] -> Board
parseBoard = map parseRow

parseRow :: String -> Row
parseRow ls = map (\x -> read x :: Int) (words ls)

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
game2048CLI row1 row2 row3 row4 movLine =
  printBoard (let board = parseBoard [row1, row2, row3, row4]
                  movement = parseMovement movLine
              in game2048 board movement)

main :: IO()
main = do
    row1 <- getLine
    row2 <- getLine
    row3 <- getLine
    row4 <- getLine
    movSymbol <- getLine
    putStrLn $ game2048CLI row1 row2 row3 row4 movSymbol

