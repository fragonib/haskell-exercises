module Kata.MarblesTree where

import Data.List.NonEmpty (NonEmpty)

data MarbleTree a =
    Leaf a
  | Parent a [MarbleTree a]
    deriving (Show)

moveMarbleDownstream :: Num a => MarbleTree a -> MarbleTree a
moveMarbleDownstream (Leaf marbles) = Leaf marbles
moveMarbleDownstream (Parent a []) = Parent (a - 1) []
moveMarbleDownstream (Parent a nodes) = Parent (a - 1) (increaseMinimum nodes) 

increaseMinimum :: Num a => [MarbleTree a] -> [MarbleTree a]
increaseMinimum [] = []
increaseMinimum [Leaf a] = [Leaf $ a + 1]  

