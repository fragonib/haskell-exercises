{-# LANGUAGE LambdaCase #-}

module Functions where
import Client

filterOnes :: (Num a, Eq a) => [a] -> [a]
filterOnes = filter (\item -> item == 1)

filterANumber :: (Num a, Eq a) => a -> [a] -> [a]
filterANumber number = filter (\item -> item == number)

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = filter (\item -> not (f item))

filterGovOrgs :: [Client] -> [Client]
filterGovOrgs = filter isGovOrg

isGovOrg = \case
  GovOrg _ -> True
  _ -> False