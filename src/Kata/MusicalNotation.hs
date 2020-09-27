module Kata.MusicalNotation where

import Data.Char

data Note =
  SimpleNote { symbol :: Char } |
  RepeatedNote { symbol :: Char , repetition :: Int }
  deriving (Show, Eq)

musicalNotationCLI :: [Char] -> [Note]
musicalNotationCLI l = foldr ((:) . symbolToNote) [] $ words l

symbolToNote :: [Char] -> Note
symbolToNote (symbol:repetition)
 | null repetition = SimpleNote symbol
 | otherwise = RepeatedNote symbol (digitToInt $ head repetition)


main :: IO()
main = do
    count <- getLine
    notes <- getLine
    mapM_ print $ musicalNotationCLI notes