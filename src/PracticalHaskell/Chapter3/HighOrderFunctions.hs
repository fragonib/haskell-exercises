{-# LANGUAGE LambdaCase #-}
module PracticalHaskell.Chapter3.HighOrderFunctions where

import qualified PracticalHaskell.Chapter2.Client as C


myFilter :: (a -> Bool) -> [a] -> [a]
myFilter predicate [] = []
myFilter predicate (item:items) =
  if predicate item
  then item : myFilter predicate items
  else myFilter predicate items

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

filterOnes :: (Num a, Eq a) => [a] -> [a]
filterOnes = filter (== 1)

filterANumber :: (Num a, Eq a) => a -> [a] -> [a]
filterANumber = filter . (==)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot = filter . (not .)
-- f(g(a))     f $ g a

filterGovOrgs :: [C.Client] -> [C.Client]
filterGovOrgs = filter isGovOrg
  where isGovOrg = \case
          C.GovOrg _ -> True
          _ -> False