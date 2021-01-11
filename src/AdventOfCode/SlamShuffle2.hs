module AdventOfCode.SlamShuffle2 where

import Data.Semigroup (stimes)

-- Data

type ShuffleCommand = String
type PileSize = Int
type Repetitions = Int
type Locus = Int

data Permutation =
  Permutation {
    scale :: Int,
    offset :: Int
  }
instance Semigroup Permutation where
  Permutation a b <> Permutation a' b' = Permutation (a' * a) (a' * b + b')

instance Monoid Permutation where
  mempty = Permutation 1 0

--instance Group Permutation where
--    invert (Permutation a b) = Permutation a' b'
--      where
--        a' = -- ??
--        b' = -- ??


-- Solve

doShuffleCommand :: PileSize -> ShuffleCommand -> Permutation
doShuffleCommand pileSize shuffleCommand =
  case words shuffleCommand of
     ["deal", "into", "new", "stack"] -> Permutation { scale = -1, offset = pileSize - 1 }
     ["cut", cutPosition] -> Permutation { scale = 1, offset = negate $ read cutPosition }
     ["deal", "with", "increment", increment] -> Permutation { scale = read increment, offset = 0 }
     _ -> error "unknown"

runPerm :: Permutation -> PileSize -> Locus -> Locus
runPerm (Permutation a b) pileSize initialLocus =
  (a * initialLocus + b) `mod` pileSize


-- IO

slamShuffle2 :: PileSize -> Repetitions -> [ShuffleCommand] -> Permutation
slamShuffle2 pileSize repetitions shuffleCommands =
  stimes repetitions combinedPerm
  where combinedPerm = mconcat $ doShuffleCommand pileSize <$> shuffleCommands

main :: IO()
main = do
  inputLines <- lines <$> getContents
  let [targetCard, pileSize, repetitions] = map read $ words $ head inputLines
      shuffleCommands = reverse $ tail inputLines
      combinedPerm = slamShuffle2 pileSize repetitions shuffleCommands
   in print $ runPerm combinedPerm pileSize targetCard