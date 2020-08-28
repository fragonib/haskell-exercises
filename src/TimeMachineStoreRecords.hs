{-# LANGUAGE RecordWildCards #-}

module TimeMachineStoreRecords where

import Client

data TimeMachine = TimeMachine
  { manufacturer :: String,
    model :: Int,
    name :: String,
    timeTravel :: TimeTravel,
    price :: Float
  }
  deriving (Show, Eq)

data TimeTravel = Past | Future | Both
  deriving (Show, Eq)

yearDiscount :: [TimeMachine] -> [TimeMachine]
yearDiscount [] = []
yearDiscount (machine : machines) =
  case machine of
    tm@TimeMachine { .. }  -> tm { price = price * 0.5 } : yearDiscount machines
