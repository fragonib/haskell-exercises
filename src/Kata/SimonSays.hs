module Kata.SimonSays where

import Data.List.Extra

-- Core

simonSays :: String -> String
simonSays phrase = 
  if isSimonOrder phraseWords
  then simonCommand phraseWords 
  else ""
  where phraseWords = words phrase

isSimonOrder :: [String] -> Bool
isSimonOrder phraseWords =
  map lower (take 2 phraseWords) == ["simon", "says"]

simonCommand :: [String] -> String
simonCommand = unwords . drop 2


-- IO

simonCLI :: [String] -> [String]
simonCLI = map simonSays

main :: IO()
main = do
  inputLines <- lines <$> getContents
  putStr $ unlines $ simonCLI $ drop 1 inputLines