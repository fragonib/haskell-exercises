{-# LANGUAGE LambdaCase #-}
module Chapter3.HighOrderFunctions where

import qualified Chapter2.Client as C


filterOnes :: (Num a, Eq a) => [a] -> [a]
filterOnes = filter (\item -> item == 1)

filterANumber :: (Num a, Eq a) => a -> [a] -> [a]
filterANumber number = filter (\item -> item == number)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (\item -> not (f item))

filterGovOrgs :: [C.Client] -> [C.Client]
filterGovOrgs = filter isGovOrg

isGovOrg = \case
  C.GovOrg _ -> True
  _ -> False

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter predicate [] = []
myFilter predicate (item:items) =
  if predicate item
  then item : myFilter predicate items
  else myFilter predicate items

