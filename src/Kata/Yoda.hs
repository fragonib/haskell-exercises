module Kata.Yoda where

import Data.Char


-- Utils

leftPadZero :: Int -> [Int] -> [Int]
leftPadZero = leftPad 0

leftPad :: Int -> Int -> [Int] -> [Int]
leftPad padInt desiredLength xs =
  replicate times padInt ++ xs
  where times = max 0 (desiredLength - length xs)

leftTruncateZero :: [Int] -> [Int]
leftTruncateZero = leftTruncate 0

leftTruncate :: Int -> [Int] -> [Int]
leftTruncate _ [] = []
leftTruncate _ [single] = [single]
leftTruncate el l@(first:rest) =
  if first == el then leftTruncate el rest else l


-- IO

convertDigitToInt :: Char -> Int
convertDigitToInt c
  | isDigit c = digitToInt c
  | otherwise = 0

convertIntToDigit :: [Int] -> [Char]
convertIntToDigit [] = "YODA"
convertIntToDigit xs = map intToDigit xs


-- Collide

collideInts :: [Int] -> [Int] -> [Int]
collideInts firstInts secondInts =
  map fst (filter intSurviveCollision digitPairs)
  where digitPairs = pairInts firstInts secondInts

intSurviveCollision :: Ord a => (a, a) -> Bool
intSurviveCollision = uncurry (>=)

pairInts :: [Int] -> [Int] -> [(Int, Int)]
pairInts firstInts secondInts =
  zip paddedFirsts paddedSeconds
  where paddedFirsts = leftPadZero (length secondInts) firstInts
        paddedSeconds = leftPadZero (length firstInts) secondInts


-- IO

yodaCLI :: [Char] -> [Char] -> [Char]
yodaCLI firstDigits secondDigits =
  convertIntToDigit $ leftTruncateZero $ collideInts firstInts secondInts
  where firstInts = map convertDigitToInt firstDigits
        secondInts = map convertDigitToInt secondDigits


main :: IO()
main = do
    firstDigits <- getLine
    secondDigits <- getLine
    putStrLn $ yodaCLI firstDigits secondDigits
    putStrLn $ yodaCLI secondDigits firstDigits