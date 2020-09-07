module Chapter3.Folds where

import qualified Chapter2.Client as C


data InfNumber a =
    MinusInfinity |
    Number a |
    PlusInfinity
      deriving Show

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr aggregator initial [] = initial
myFoldr aggregator initial (x:xs) = aggregator x acum
  where acum = myFoldr aggregator initial xs


-- Product matching functions

productPM :: Num a => [a] -> a
productPM [] = 1
productPM (x:xs) = x * productPM xs

minimumClientPM :: [C.Client] -> C.Client
minimumClientPM [client] = client
minimumClientPM (client:clients) =
  if length (clientName client) < length (clientName restMinimumClient)
  then client
  else restMinimumClient
    where restMinimumClient = minimumClientPM clients

clientName :: C.Client -> [Char]
clientName (C.Individual (C.Person name _ _) _) = name
clientName (C.Company name _ _ _) = name
clientName (C.GovOrg name) = name

allPM :: [Bool] -> Bool
allPM [condition] = condition
allPM (condition:conditions) = condition && allPM conditions
