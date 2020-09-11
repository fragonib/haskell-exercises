{-# LANGUAGE RecordWildCards #-}
module Chapter3.ClientRecords where

data Client = 
  GovOrg     { name :: String } |
  Company    { name :: String, 
               nif :: Integer, 
               ceo :: Person, 
               role :: String } |
  Individual { person :: Person, boss :: Bool }
  deriving (Show, Eq)

data Person = 
  Person { firstName :: String, 
           surname :: String, 
           gender :: Gender }
  deriving (Show, Eq)

data Gender = 
  Male | 
  Female | 
  Unknown
  deriving (Show, Eq)