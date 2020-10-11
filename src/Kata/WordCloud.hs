module Kata.WordCloud where

-- Word Cloud

calculateCloudHeight :: Int -> Int -> [(String, Int)] -> Int
calculateCloudHeight maxWidth maxWords histogram = 5


-- IO

cloudSize :: String -> [Int]
cloudSize firstLine = map read $ words firstLine

wordHistogram :: [String] -> [(String, Int)]
wordHistogram = map wordOccurrences 

wordOccurrences :: String -> (String, Int)
wordOccurrences ls = 
  case words ls of
    (word:frequency:_) -> (word, read frequency)

outputHeight :: Int -> Int -> String
outputHeight index height = "CLOUD " ++ show index ++ ": " ++ show height 

wordCloudCLI :: [String] -> String
wordCloudCLI inputLines =
  let maxWidth = head (cloudSize (head inputLines))
      maxWords = head $ tail $ cloudSize (head inputLines)
      histogram = wordHistogram $ takeWhile (/= "0 0") $ tail inputLines
      cloudHeight = calculateCloudHeight maxWidth maxWords histogram
   in outputHeight 1 cloudHeight

main :: IO()
main = do
  inputLines <- lines <$> getContents
  putStrLn $ wordCloudCLI inputLines
