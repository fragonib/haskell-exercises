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
     ["deal", "into", "new", "stack"] -> shuffleByRestackingPerm pileSize
     ["cut", position] -> shuffleByCuttingPerm pileSize (read position)
     ["deal", "with", "increment", increment] -> shuffleWithIncrementPerm pileSize (read increment)
     _ -> error "unknown"

shuffleByRestackingPerm :: PileSize -> Permutation
shuffleByRestackingPerm pileSize =
  Permutation (\locus -> pileSize - locus - 1)

shuffleByCuttingPerm :: PileSize -> CutPosition -> Permutation
shuffleByCuttingPerm pileSize cutPosition =
  Permutation (\locus -> if locus < positiveCutPosition
                  then locus + (pileSize - positiveCutPosition)
                  else locus - positiveCutPosition)
  where positiveCutPosition = if cutPosition < 0
                              then pileSize + cutPosition
                              else cutPosition

shuffleWithIncrementPerm :: PileSize -> Increment -> Permutation
shuffleWithIncrementPerm pileSize increment =
  Permutation (\locus -> (locus * increment) `mod` pileSize)

-- IO

cardLocusAfterShufflingPerm :: PileSize -> [ShuffleCommand] -> Permutation
cardLocusAfterShufflingPerm pileSize =
  mconcat . map (doShuffleCommand pileSize)

main :: IO()
main = do
  inputLines <- lines <$> getContents
  let [targetCard, pileSize] = map read $ words $ head inputLines
      shuffleCommands = tail inputLines
      combinedPerm = cardLocusAfterShufflingPerm pileSize shuffleCommands
   in print $ runPerm combinedPerm targetCard