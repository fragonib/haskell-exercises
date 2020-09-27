module Kata.MusicalNotation where

import Data.Char
import Data.List (intercalate)


data Note =
  Note { symbol :: Char, repetition :: Int }
  deriving (Show, Eq)

data NoteLine =
  NoteLine { sign :: Char, separator :: Char }
  deriving (Show, Eq)

noteSymbols = [
 NoteLine 'G' ' ',
 NoteLine 'F' '-',
 NoteLine 'E' ' ',
 NoteLine 'D' '-',
 NoteLine 'C' ' ',
 NoteLine 'B' '-',
 NoteLine 'A' ' ',
 NoteLine 'g' '-',
 NoteLine 'f' ' ',
 NoteLine 'e' '-',
 NoteLine 'd' ' ',
 NoteLine 'c' ' ',
 NoteLine 'b' ' ',
 NoteLine 'a' '-'
 ]

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

musicLine :: NoteLine -> [Note] -> [Char]
musicLine noteLine noteSequence =
  musicLinePrefix noteLine ++
  musicLineContent noteLine noteSequence

musicLinePrefix :: NoteLine -> [Char]
musicLinePrefix noteLine = sign noteLine : ": "

musicLineContent :: NoteLine -> [Note] -> [Char]
musicLineContent noteLine noteSequence = intercalate [separator noteLine] $
  map (noteSymbolToNotePart noteLine) noteSequence

noteSymbolToNotePart :: NoteLine -> Note -> [Char]
noteSymbolToNotePart noteLine singleNote = case singleNote of
   Note { symbol = s, repetition = r } -> replicate r pitch
                                          where pitch = if sign noteLine == s then '*' 
                                                        else separator noteLine


main :: IO()
main = do
    count <- getLine
    notes <- getLine
    putStrLn $ musicalNotationCLI notes