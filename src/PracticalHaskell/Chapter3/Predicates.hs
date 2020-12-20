{-# LANGUAGE RecordWildCards #-}
module PracticalHaskell.Chapter3.Predicates where

import Data.List
import PracticalHaskell.Chapter3.ClientRecords
import Data.Function (on)


-- Is elem present
elem :: (Eq a) => a -> [a] -> Bool
elem item list = case find (item ==) list of
  Just _ -> True
  _ -> False



-- Companies duties

companyDutiesAnalytics :: [Client] -> [String] 
companyDutiesAnalytics =
  map (duty . head) .
  sortBy (\x y -> compare (length y) (length x)) .
  groupBy (\x y -> duty x == duty y) .
  filter isCompany

-- Transform in PointFree (PF) using following functions:
-- Data.Function.on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- flip :: (a -> b -> c) -> b -> a -> c

companyDutiesAnalyticsPF :: [Client] -> [String] 
companyDutiesAnalyticsPF =
  map (duty . head) .
  sortBy (flip compare `on` length) .
  groupBy ((==) `on` duty) .
  filter isCompany

isCompany :: Client -> Bool                          
isCompany Company {} = True 
isCompany _ = False

duty :: Client -> String
duty Company { name = "ACME" } = "35%"
duty Company { name = "POOL" } = "20%"
duty Company { name = "AQQA" } = "20%"
duty _ = "0%"