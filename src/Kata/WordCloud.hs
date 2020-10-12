module Kata.WordCloud where

import Data.List (sortOn)
import Kata.Utils (trace1, trace2)


type WordHistogram = [WordFreq]
type WordFreq = (String, Int)
type WordBox = (Int, Int) -- (Height, Width)


-- Word Cloud

calculateCloudHeight :: Int -> Int -> WordHistogram -> Int
calculateCloudHeight maxLineWidth maxWords wordHistogram =
  sum $
  cloudLinesHeight maxLineWidth $
  wordBoxes $
  targetWords maxWords wordHistogram

targetWords :: Int -> WordHistogram -> WordHistogram
targetWords maxWords wordHistogram =
  take maxWords $
  sortOn fst $
  filter ((>=5) . snd) wordHistogram

wordBoxes :: WordHistogram -> [WordBox]
wordBoxes wordHistogram =
  map (wordBox maxFreq) wordHistogram
  where maxFreq = maximum $ map snd wordHistogram

cloudLinesHeight :: Int -> [WordBox] -> [Int]
cloudLinesHeight maxLineWidth wordBoxes =
  case wordBoxes of
    [] -> []
    _  -> lineHeight : cloudLinesHeight maxLineWidth wordsThatNotFitInLine
          where (lineHeight, countOfWordsThatFit) = cloudLineHeight maxLineWidth wordBoxes
                wordsThatNotFitInLine = drop countOfWordsThatFit wordBoxes

cloudLineHeight :: Int -> [WordBox] -> (Int, Int)
cloudLineHeight maxLineWidth wordBoxes =
  (maxWordHeight, countOfWordsThatFitInLine)
  where maxWordHeight = maximum $ take countOfWordsThatFitInLine $ map fst wordBoxes
        countOfWordsThatFitInLine = wordsThatFitInLine maxLineWidth $ map snd wordBoxes

wordsThatFitInLine :: Int -> [Int] -> Int
wordsThatFitInLine maxLineWidth wordWidths =
  length $
  takeWhile (<= maxLineWidth) $
  scanl1 (+) $
  addSpacer 10 $
  wordWidths

addSpacer :: Int -> [Int] -> [Int]
addSpacer _ [] = []
addSpacer _ [x] = [x]
addSpacer spacePts (x:ls) = x : map (+ spacePts) ls

wordBox :: Int -> WordFreq -> WordBox
wordBox maxFreq (word, wordFreq) =
  (fontSize, width)
  where fontSize = wordHeightInPts maxFreq wordFreq
        width = wordWidthInPts word fontSize

maxWordFreq :: WordHistogram -> Int
maxWordFreq wordHistogram = maximum $ map snd wordHistogram

wordHeightInPts :: Int -> Int -> Int
wordHeightInPts maxOccurrences actualOccurrences =
  8 + ((40 * (actualOccurrences - 4)) `ceilDiv` (maxOccurrences - 4))

wordWidthInPts :: [Char] -> Int -> Int
wordWidthInPts word fontSize =
  (9 * length word * fontSize) `ceilDiv` 16

ceilDiv :: Int -> Int -> Int
a `ceilDiv` b = (a + b - 1) `div` b


-- IO

extractCloudSize :: String -> [Int]
extractCloudSize firstLine = map read $ words firstLine

extractWordHistogram :: [String] -> WordHistogram
extractWordHistogram = map extractWordOccurrences

extractWordOccurrences :: String -> WordFreq
extractWordOccurrences ls =
  case words ls of
     (word:frequency:_) -> (word, read frequency)

outputHeight :: Int -> Int -> String
outputHeight index height = "CLOUD " ++ show index ++ ": " ++ show height

wordCloudCLI :: [String] -> String
wordCloudCLI inputLines =
  let maxWidth = head (extractCloudSize (head inputLines))
      maxWords = head $ tail $ extractCloudSize (head inputLines)
      wordHistogram = extractWordHistogram $ takeWhile (/= "0 0") $ tail inputLines
      cloudHeight = calculateCloudHeight maxWidth maxWords wordHistogram
   in outputHeight 1 cloudHeight

main :: IO()
main = do
  inputLines <- lines <$> getContents
  putStrLn $ wordCloudCLI inputLines
