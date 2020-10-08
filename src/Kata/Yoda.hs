module Kata.Yoda where

import Data.Char
import qualified Kata.Utils as U


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
  where paddedFirsts = U.leftPadZero (length secondInts) firstInts
        paddedSeconds = U.leftPadZero (length firstInts) secondInts


-- IO

yodaCLI :: [Char] -> [Char] -> [Char]
yodaCLI firstDigits secondDigits =
  convertIntToDigit $ U.leftTruncateZero $ collideInts firstInts secondInts
  where firstInts = map convertDigitToInt firstDigits
        secondInts = map convertDigitToInt secondDigits


convertDigitToInt :: Char -> Int
convertDigitToInt c
  | isDigit c = digitToInt c
  | otherwise = 0

convertIntToDigit :: [Int] -> [Char]
convertIntToDigit [] = "YODA"
convertIntToDigit xs = map intToDigit xs


-- CLI

main :: IO()
main = do
    firstDigits <- getLine
    secondDigits <- getLine
    putStrLn $ yodaCLI firstDigits secondDigits
    putStrLn $ yodaCLI secondDigits firstDigits