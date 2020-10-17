module Kata.Backslash where

import qualified Kata.Utils as U


-- Backslash

scapeText :: Int -> String -> String
scapeText levels text =
  case levels of
    0 -> text
    1 -> concatMap scapeOne text
    _ -> concatMap scapeOne $ scapeText (levels - 1) text

scapeOne :: Char -> [Char]
scapeOne c
  | isSpecial c = '\\' : [c]
  | otherwise = [c]

isSpecial :: Char -> Bool
isSpecial = ((>= '!') `U.and` (<= '*')) `U.or` ((>= '[') `U.and` (<= ']'))

-- IO

backslashCLI :: [String] -> [String]
backslashCLI inputLines =
  case inputLines of
    [] -> []
    _ -> scapeText levels textLine : backslashCLI restLines
    where levels = read $ head inputLines
          textLine = head $ tail inputLines
          restLines = drop 2 inputLines

main :: IO()
main = do
  inputLines <- lines <$> getContents
  putStr $ unlines $ backslashCLI inputLines