module Kata.MusicalNotation where

import Data.Char
import Data.List (intercalate)

data Note =
  Note { symbol :: Char , repetition :: Int }
  deriving (Show, Eq)


musicalNotationCLI :: [Char] -> [Note]
musicalNotationCLI l = foldr ((:) . symbolToNote) [] $ words l

symbolToNote :: [Char] -> Note
symbolToNote (symbol:repetition)
 | null repetition = Note symbol 1
 | otherwise = Note symbol (digitToInt $ head repetition)

musicLine :: Char -> [Note] -> [Char]
musicLine noteSymbol noteSequence = musicLinePrefix noteSymbol ++ musicLineContent noteSymbol noteSequence

musicLinePrefix :: Char -> [Char]
musicLinePrefix noteSymbol = noteSymbol : ": "

musicLineContent :: Char -> [Note] -> [Char]
musicLineContent symbol notes = intercalate "-" $ map (noteSymbolToNotePart symbol) notes

noteSymbolToNotePart :: Char -> Note -> [Char]
noteSymbolToNotePart noteSymbol note = case note of
   Note { symbol = s, repetition = r } -> replicate r pitch
                                          where pitch = if noteSymbol == s then '*' else '-'


main :: IO()
main = do
    count <- getLine
    notes <- getLine
    putStrLn $ musicLine 'C' $ musicalNotationCLI notes