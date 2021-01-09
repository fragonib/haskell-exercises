module AdventOfCode.SlamShuffle2 where

import AdventOfCode.SlamShuffle (doShuffleCommand, runPerm, PileSize, ShuffleCommand, Permutation)
import Data.Semigroup (stimes)

type Repetitions = Int

-- IO

slamShuffle2 :: PileSize -> Repetitions -> [ShuffleCommand] -> Permutation
slamShuffle2 pileSize repetitions shuffleCommands =
  stimes repetitions combinedPerm
  where combinedPerm = mconcat $ doShuffleCommand pileSize <$> shuffleCommands

main :: IO()
main = do
  inputLines <- lines <$> getContents
  let [targetCard, pileSize, repetitions] = map read $ words $ head inputLines
      shuffleCommands = tail inputLines
      combinedPerm = slamShuffle2 pileSize repetitions shuffleCommands
   in print $ runPerm combinedPerm targetCard