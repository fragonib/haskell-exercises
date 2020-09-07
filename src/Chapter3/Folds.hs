module Chapter3.Folds where

import qualified Chapter2.Client as C


-- Fold 

data InfNumber a =
    MinusInfinity |
    Number a |
    PlusInfinity
      deriving Show

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ initial [] = initial
myFoldr aggregator initial (x:xs) = aggregator x acum
  where acum = myFoldr aggregator initial xs

-- Pattern Matching functions

productPM :: Num a => [a] -> a
productPM [] = 1
productPM (x:xs) = x * productPM xs

minimumClientPM :: [C.Client] -> C.Client
minimumClientPM [client] = client
minimumClientPM (client:clients) = minClient client (minimumClientPM clients)

allPM :: [Bool] -> Bool
allPM [condition] = condition
allPM (condition:conditions) = condition && allPM conditions

-- Folding functions

productFold :: Num a => [a] -> a
productFold = foldr (*) 1

minimumClientFold :: [C.Client] -> C.Client
minimumClientFold = foldr1 minClient

allFold :: [Bool] -> Bool
allFold = foldr (&&) True

minimumBy :: (Ord a) => (a -> a) -> [a] -> a
minimumBy by = foldr1 (\x y -> if by x < by y then x else y)


-- Client comparison

minClient :: C.Client -> C.Client -> C.Client
minClient firstClient secondClient =
  if length (clientName firstClient) < length (clientName secondClient)
  then firstClient else secondClient

clientName :: C.Client -> [Char]
clientName (C.Individual (C.Person name _ _) _) = name
clientName (C.Company name _ _ _) = name
clientName (C.GovOrg name) = name
