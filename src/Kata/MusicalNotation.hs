module Kata.MusicalNotation where

import Data.Char
import Data.List (intercalate)


data InputNote =
  InputNote { symbol :: Char,
              repetitions :: Int }
  deriving (Show, Eq)

data NoteLine =
  NoteLine { sign :: Char,
             separator :: Char }
  deriving (Show, Eq)

noteLines = [
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
musicalNotationCLI notesLiteralSeq = intercalate "\n" $
  map (`musicLine` notesList) noteLines
  where notesList = parseNotes notesLiteralSeq


-- Parsing input

parseNotes :: [Char] -> [InputNote]
parseNotes notesLiteralSeq = foldr ((:) . symbolToNote) [] $ words notesLiteralSeq

symbolToNote :: [Char] -> InputNote
symbolToNote (symbol:repetitions)
 | null repetitions = InputNote symbol 1
 | otherwise = InputNote symbol (digitToInt $ head repetitions)

-- Generate output

musicLine :: NoteLine -> [InputNote] -> [Char]
musicLine noteLine noteSequence =
  musicLinePrefix noteLine ++
  musicLineContent noteLine noteSequence

musicLinePrefix :: NoteLine -> [Char]
musicLinePrefix noteLine = sign noteLine : ": "

musicLineContent :: NoteLine -> [InputNote] -> [Char]
musicLineContent noteLine noteSequence = intercalate [separator noteLine] $
  map (noteSymbolToNotePart noteLine) noteSequence

noteSymbolToNotePart :: NoteLine -> InputNote -> [Char]
noteSymbolToNotePart noteLine (InputNote symbol repetitions) =
  replicate repetitions pitch
  where pitch = if sign noteLine == symbol then '*'
                else separator noteLine

-- CLI

main :: IO()
main = do
    count <- getLine
    notes <- getLine
    putStrLn $ musicalNotationCLI notes