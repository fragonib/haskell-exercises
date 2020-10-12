module Kata.WordCloud where

import Data.List (sortOn)


type WordHistogram = [WordFreq]
type WordFreq = (String, Int)
type WordSize = (Int, Int) -- FontSize, Width


-- Word Cloud

calculateCloudHeight :: Int -> Int -> WordHistogram -> Int
calculateCloudHeight maxLineWidth maxWords wordHistogram =
  sum $
  cloudLineHeights maxLineWidth $
  wordsInCloud maxWords wordHistogram

wordsInCloud :: Int -> WordHistogram -> WordHistogram
wordsInCloud maxWords wordHistogram =
  take maxWords $
  sortOn fst $
  filter ((<5) . snd) wordHistogram

cloudLineHeights :: Int -> WordHistogram -> [Int]
cloudLineHeights maxLineWidth wordHistogram =
  lineHeight : cloudLineHeights maxLineWidth (drop wordsCount wordHistogram)
  where (lineHeight, wordsCount) = cloudLineHeight maxLineWidth wordHistogram

cloudLineHeight :: Int -> WordHistogram -> (Int, Int)
cloudLineHeight maxLineWidth wordHistogram = (maxWordHeight, wordsCountThatFitInLine)
  where maxWordHeight = maximum $ take wordsCountThatFitInLine $ map fst wordSizesList
        wordsCountThatFitInLine = wordsThatFitInLine maxLineWidth wordSizesList
        wordSizesList = wordSizes wordHistogram

wordSizes :: WordHistogram -> [WordSize]
wordSizes wordHistogram =
  map (wordSizeHeight maxFreq) wordHistogram
    where maxFreq = maxWordFreq wordHistogram

wordsThatFitInLine :: Int -> [WordSize] -> Int
wordsThatFitInLine maxLineWidth wordSizes =
  length $
  takeWhile (< maxLineWidth) $
  scanl (+) 0 $
  map snd wordSizes

wordSizeHeight :: Int -> WordFreq -> (Int, Int)
wordSizeHeight maxFreq (word, wordFreq) =
  let fontSize = wordHeightInPts maxFreq wordFreq
      width = wordWidthInPts word fontSize
   in (fontSize, width)

maxWordFreq :: WordHistogram -> Int
maxWordFreq ls = maximum $ map snd ls

wordHeightInPts :: Int -> Int -> Int
wordHeightInPts maxOccurrences actualOccurrences =
  8 + (40 * (actualOccurrences - 4)) `ceilDiv` (maxOccurrences - 4)

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
extractWordOccurrences ls = case words ls of (word:frequency:_) -> (word, read frequency)

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
