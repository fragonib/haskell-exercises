module PracticalHaskell.TimeMachine.Store where

data TimeMachine = TimeMachine Manufacturer Model Name TimeTravel Price
            deriving (Show, Eq)
newtype Manufacturer = Manufacturer String
            deriving (Show, Eq)
newtype Model = Model Int
            deriving (Show, Eq)
data TimeTravel = Past | Future
            deriving (Show, Eq)
type Price = Float
type Name = String

yearDiscount :: [TimeMachine] -> [TimeMachine]
yearDiscount [] = []
yearDiscount (machine:machines) =
  case machine of
    TimeMachine manufacturer model name timeTravel price -> 
      TimeMachine manufacturer model name timeTravel (price*0.5) : yearDiscount machines
    