module AdventOfCode.SlamShuffle where

import Control.Monad.State (State)
import qualified Kata.Utils as K

type Deck = [Int]
type EnumeratedDeck = [(Int, Int)]
type ShuffleCommand = String
type Size = Int

factoryDeck :: Size -> Deck
factoryDeck n = [0 .. n - 1]

-- Shuffling

doShuffleCommand :: ShuffleCommand -> Deck -> Deck
doShuffleCommand shuffleCommand =
  case words shuffleCommand of
     ["deal", "with", "new", "stack"] -> restackDeck
     ["deal", "with", "increment", position] -> dealWithIncrement $ read position
     ["cut", position] -> cutDeck $ read position

restackDeck :: Deck -> Deck
restackDeck = reverse

cutDeck :: Int -> Deck -> Deck
cutDeck locus deck =
  pile2 ++ pile1
  where cutPosition = if locus < 0 then length deck + locus else locus
        (pile1, pile2) = splitAt cutPosition deck

dealWithIncrement :: Int -> Deck -> Deck
dealWithIncrement increment deck =
  snd <$> dealWithIncrementEnumerated pileSize increment enumeratedDeck
  where
    enumeratedDeck = K.enumerate deck
    pileSize = length deck

dealWithIncrementEnumerated :: Int -> Int -> EnumeratedDeck -> EnumeratedDeck
dealWithIncrementEnumerated pileSize increment enumeratedDeck@(x:xs) =
    (enumeratedDeck !! index) : dealWithIncrementEnumerated pileSize increment xs
    where
      index = nth + (pileSize - length enumeratedDeck)
      nth = invertPerm pileSize increment (fst x)

invertPerm :: Int -> Int -> Int -> Int
invertPerm pileSize increment j
  | j `mod` increment == 0 = j `div` increment
  | otherwise = invertPerm pileSize increment (j+pileSize)

-- IO

shuffleCLI :: [String] -> String
shuffleCLI shuffleCommands =
  show shuffledDeck
  where shuffledDeck = foldl (flip doShuffleCommand) (factoryDeck 10) shuffleCommands

main :: IO()
main = do
  inputLines <- lines <$> getContents
  putStr $ shuffleCLI inputLines
