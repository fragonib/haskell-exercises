module AdventOfCode.RocketEquation where

type Mass = Int
type Fuel = Int

fuelForModule :: Mass -> Fuel
fuelForModule mass = mass `div` 3 - 2

extraFuel :: Mass -> Fuel
extraFuel 0 = 0
extraFuel fuelMass =
  fuelNeeded + extraFuel fuelNeeded
  where fuelNeeded = max (fuelForModule fuelMass) 0

totalFuel :: Mass -> Fuel
totalFuel mass = mainFuel + extraFuel mainFuel
  where mainFuel = fuelForModule mass

main :: IO()
main = do
  inputLines <- lines <$> getContents
  print $ sum $ totalFuel . read <$> inputLines


