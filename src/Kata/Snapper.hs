module Kata.Snapper where

import Data.List (words)
import Kata.Utils (enumerateStartingWith)


type Snappers = Int
type Snaps = Int
type CaseIndex = Int
type LightState = Bool

data Led =
  ON | OFF
  deriving (Show, Eq)


-- Core

isLightOn :: Snappers -> Snaps -> LightState
isLightOn ds sn = True

-- IO

extractDevicesAndSnaps :: String -> (Snappers, Snaps)
extractDevicesAndSnaps line =
  (head digits, digits !! 1)
  where digits = map read $ words line

outputLightState :: CaseIndex -> LightState -> String
outputLightState index state = 
  "Case #" ++ show index ++ ": " ++ (if state then "ON" else "OFF")

snapperCLI :: [String] -> [String]
snapperCLI caseLines =
  map (uncurry outputLightState) $
  zip [1..] $
  map (uncurry isLightOn) $
  map extractDevicesAndSnaps $
  caseLines

main :: IO()
main = do
  inputLines <- lines <$> getContents
  putStr $ unlines $ snapperCLI $ drop 1 inputLines