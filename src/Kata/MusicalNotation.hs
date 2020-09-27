module Kata.MusicalNotation where

import Data.Char

data Note =
  SimpleNote { symbol :: Char } |
  RepeatedNote { symbol :: Char , repetition :: Int }
  deriving (Show, Eq)

musicalNotationCLI :: [Char] -> [Note]
musicalNotationCLI l = foldr symbolToNote [] $ words l

symbolToNote :: [Char] -> [Note] -> [Note]
symbolToNote (symbol:repetition) notes
 | null repetition = SimpleNote symbol : notes
 | otherwise = RepeatedNote symbol (digitToInt $ head repetition) : notes


main :: IO()
main = do
    l1 <- getLine
    l2 <- getLine
    mapM_ print $ musicalNotationCLI $ l2