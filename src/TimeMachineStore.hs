module TimeMachineStore where

import Client

data TimeMachine = TimeMachine Manufacturer Model Name TimeTravel Price
            deriving Show
newtype Manufacturer = Manufacturer String
            deriving Show
newtype Model = Model Int
            deriving Show
data TimeTravel = Past | Future
            deriving Show
type Price = Float
type Name = String
