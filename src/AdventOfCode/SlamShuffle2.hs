{-# LANGUAGE ScopedTypeVariables #-}

module AdventOfCode.SlamShuffle2 where

import Data.Semigroup (stimes)
import Data.Finite (Finite, modulo)
import GHC.TypeLits (KnownNat)
import Data.Group

-- Data

type ShuffleCommand = String
type PileSize = Int
type Repetitions = Int
type Locus = Int

-- Solve

data Permutation =
  Perm {
    pileSize :: Int,
    scale :: Int,
    offset :: Int
  }
  deriving (Show)

instance Semigroup Permutation where
  Perm s a b <> Perm s' a' b' =
    let realSize = max s s'
     in Perm realSize (a' * a `mod` realSize) ((a' * b + b') `mod` realSize)

instance Monoid Permutation where
  mempty = Perm { pileSize = 2, scale = 1, offset = 0 }

instance Group Permutation where
  invert (Perm s a b) = Perm s' a' b'
      where
        s' = s
        a' = a ^ (s - 2)
        b' = negate (a' * b)

doShuffleCommand :: PileSize -> ShuffleCommand -> Permutation
doShuffleCommand ps shuffleCommand =
  case words shuffleCommand of
     ["deal", "into", "new", "stack"] -> Perm { pileSize = ps, scale = -1, offset = ps - 1 }
     ["cut", cutPosition] -> Perm { pileSize = ps, scale = 1, offset = (-read cutPosition) `mod` ps }
     ["deal", "with", "increment", increment] -> Perm { pileSize = ps, scale = read increment, offset = 0 }
     _ -> error "Unknown command"

runPerm :: Permutation -> Locus -> Locus
runPerm (Perm s a b) initialLocus = (a * initialLocus + b) `mod` s

-- IO

slamShuffle2 :: PileSize -> Repetitions -> [ShuffleCommand] -> Permutation
slamShuffle2 ps repetitions shuffleCommands =
  stimes repetitions combinedPerm
  where combinedPerm = mconcat $ doShuffleCommand ps <$> shuffleCommands

main :: IO()
main = do
  inputLines <- lines <$> getContents
  let [targetCard, ps, repetitions] = map read $ words $ head inputLines
      shuffleCommands = reverse $ tail inputLines
      combinedPerm = slamShuffle2 ps repetitions shuffleCommands
   in print $ runPerm combinedPerm targetCard