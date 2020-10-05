{-# LANGUAGE NamedFieldPuns #-}
module Kata.MusicalNotation where

import Data.Char
import Data.List (intercalate)


data Note =
  Note { symbol :: Char, 
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

parseNotes :: [Char] -> [Note]
parseNotes notesLiteralSeq = foldr ((:) . symbolToNote) [] $ words notesLiteralSeq

symbolToNote :: [Char] -> Note
symbolToNote (symbol:repetitions)
 | null repetitions = Note symbol 1
 | otherwise = Note symbol (digitToInt $ head repetitions)

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
noteSymbolToNotePart noteLine singleNote =
  case singleNote of
    Note { symbol, repetitions } -> replicate repetitions pitch
                                   where pitch = if sign noteLine == symbol then '*'
                                                 else separator noteLine

-- CLI

main :: IO()
main = do
    count <- getLine
    notes <- getLine
    putStrLn $ musicalNotationCLI notes