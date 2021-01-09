module AdventOfCode.SlamShuffle where

import Control.Monad.State (State)
import qualified Kata.Utils as K
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

type Deck = [Int]
type EnumeratedDeck = [(Int, Int)]
type ShuffleCommand = String
type PileSize = Int

factoryDeck :: PileSize -> Deck
factoryDeck n = [0 .. n - 1]

-- Shuffling

doShuffleCommand :: Deck -> ShuffleCommand -> Deck
doShuffleCommand initialDeck shuffleCommand =
  case words shuffleCommand of
     ["deal", "with", "new", "stack"] -> shuffleByRestacking initialDeck
     ["deal", "with", "increment", position] -> shuffleWithIncrement (read position) initialDeck
     ["cut", position] -> shuffleByCutting (read position) initialDeck

shuffleByRestacking :: Deck -> Deck
shuffleByRestacking = reverse

shuffleByCutting :: Int -> Deck -> Deck
shuffleByCutting locus deck =
  pile2 ++ pile1
  where cutPosition = if locus < 0 then length deck + locus else locus
        (pile1, pile2) = splitAt cutPosition deck

shuffleWithIncrement :: Int -> Deck -> Deck
shuffleWithIncrement increment deck =
    map (\index -> deck !! invertPerm pileSize increment index) [0..pileSize-1]
    where pileSize = length deck

invertPerm :: Int -> Int -> Int -> Int
invertPerm pileSize increment destinationLocus
  | destinationLocus `mod` increment == 0 = destinationLocus `div` increment
  | otherwise = invertPerm pileSize increment (destinationLocus + pileSize)

-- IO

cardLocusAfterShuffling :: Int -> PileSize -> [ShuffleCommand] -> Int
cardLocusAfterShuffling targetCard pileSize shuffleCommands =
  fromMaybe 0 $ elemIndex targetCard shuffledDeck
  where shuffledDeck = foldl doShuffleCommand (factoryDeck pileSize) shuffleCommands

main :: IO()
main = do
  inputLines <- lines <$> getContents
  let [targetCard, pileSize] = map read $ words $ head inputLines
      shuffleCommands = tail inputLines
   in putStrLn $ show $ cardLocusAfterShuffling targetCard pileSize shuffleCommands
