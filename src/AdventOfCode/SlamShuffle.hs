module AdventOfCode.SlamShuffle where

-- Data

type ShuffleCommand = String
type PileSize = Int
type Increment = Int
type CutPosition = Int
type Locus = Int

newtype Permutation = Permutation { runPerm :: Locus -> Locus }

instance Semigroup Permutation where
  Permutation f <> Permutation g = Permutation (g . f)

instance Monoid Permutation where
  mempty = Permutation id

-- Shuffling

doShuffleCommand :: PileSize -> ShuffleCommand -> Permutation
doShuffleCommand pileSize shuffleCommand =
  case words shuffleCommand of
     ["deal", "into", "new", "stack"] -> shuffleByRestacking pileSize
     ["cut", position] -> shuffleByCutting pileSize (read position)
     ["deal", "with", "increment", increment] -> shuffleWithIncrement pileSize (read increment)
     _ -> error "Unknown command"

shuffleByRestacking :: PileSize -> Permutation
shuffleByRestacking pileSize =
  Permutation (\locus -> -1 * locus + pileSize - 1)

shuffleByCutting :: PileSize -> CutPosition -> Permutation
shuffleByCutting pileSize cutPosition =
  Permutation (\locus -> (1 * locus - cutPosition) `mod` pileSize)

shuffleWithIncrement :: PileSize -> Increment -> Permutation
shuffleWithIncrement pileSize increment =
  Permutation (\locus -> (increment * locus) `mod` pileSize)

-- IO

slamShuffle :: PileSize -> [ShuffleCommand] -> Permutation
slamShuffle pileSize shuffleCommands =
  mconcat $ map (doShuffleCommand pileSize) shuffleCommands

main :: IO()
main = do
  inputLines <- lines <$> getContents
  let [targetCard, pileSize] = map read $ words $ head inputLines
      shuffleCommands = tail inputLines
      combinedPerm = slamShuffle pileSize shuffleCommands
   in print $ runPerm combinedPerm targetCard