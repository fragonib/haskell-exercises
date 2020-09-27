module Kata.MusicalNotation where

import Data.Char
import Data.List (intercalate)


data Note =
  Note { symbol :: Char , repetition :: Int }
  deriving (Show, Eq)

noteSymbols = ['G', 'F', 'E', 'D', 'C', 'B', 'A', 'g', 'f', 'e', 'd', 'c', 'b', 'a']

musicalNotationCLI :: [Char] -> [Char]
musicalNotationCLI notesLiteral = intercalate "\n" $
  map (`musicLine` notesList) noteSymbols
  where notesList = parseNotes notesLiteral


-- Parsing input

parseNotes :: [Char] -> [Note]
parseNotes notesLiteral = foldr ((:) . symbolToNote) [] $ words notesLiteral

symbolToNote :: [Char] -> Note
symbolToNote (symbol:repetition)
 | null repetition = Note symbol 1
 | otherwise = Note symbol (digitToInt $ head repetition)
 
-- Generate output

musicLine :: Char -> [Note] -> [Char]
musicLine noteSymbol noteSequence = 
  musicLinePrefix noteSymbol ++ 
    musicLineContent noteSymbol noteSequence

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
    putStrLn $ musicalNotationCLI notes