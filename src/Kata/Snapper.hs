module Kata.Snapper where

import Kata.Utils (trace1)


type SnapperCount = Int
type Snaps = Int
type CaseIndex = Int
type LightState = Bool

type Snapper = (Power, State)

data Power =
  UP | DOWN
  deriving (Show, Eq)

data State =
  ON | OFF
  deriving (Show, Eq)


-- Core

snapAll :: SnapperCount -> Snaps -> Snapper
snapAll snapperCount snapsTimes =
  head $
  applyNtimes snapsTimes (trace1 snap) $
  initialSnapperChain snapperCount

initialSnapperChain :: SnapperCount -> [Snapper]
initialSnapperChain snapperCount =
  reverse $ (UP, OFF) : replicate (snapperCount - 1) (DOWN, OFF)

snap :: [Snapper] -> [Snapper]
snap [] = []
snap [(_, status)] =
  case status of
    ON -> [(UP, OFF)]
    OFF -> [(UP, ON)]
snap (front:previous:rest) =
  (powered, state) : snap (previous:rest)
  where powered = if isLightOn previous then UP else DOWN
        state = if isLightOn previous then switch (snd front) else state

isLightOn :: Snapper -> Bool
isLightOn (power, state) =
  case (power, state) of
    (UP, ON) -> True
    _ -> False

switch :: State -> State
switch state =
  case state of
    ON -> OFF
    OFF -> ON

-- Utils

applyNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)


-- IO

extractDevicesAndSnaps :: String -> (SnapperCount, Snaps)
extractDevicesAndSnaps line =
  (head digits, digits !! 1)
  where digits = map read $ words line

outputLightState :: CaseIndex -> Snapper -> String
outputLightState index lastSnapper =
  "Case #" ++ show index ++ ": " ++ show lastSnapper

snapperCLI :: [String] -> [String]
snapperCLI caseLines =
  map (uncurry outputLightState) $
  zip [1..] $
  map (uncurry snapAll) $
  map extractDevicesAndSnaps $
  caseLines

main :: IO()
main = do
  inputLines <- lines <$> getContents
  putStr $ unlines $ snapperCLI $ drop 1 inputLines