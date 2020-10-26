module Kata.Snapper where

import Kata.Utils (trace1)


type Snappers = Int
type Snaps = Int
type CaseIndex = Int
type LightState = Bool

data Snapper =
  ON | OFF
  deriving (Show, Eq)

-- Core

--isLightOn :: Snappers -> Snaps -> LightState
--isLightOn dev snaps =
--  case dev of
--    1 -> odd snaps
--    _ -> isInputPower && isOnState
--         where isInputPower = isLightOn (dev - 1) (snaps - 1)
--               isOnState = isLightOn (dev - 1) (snaps - 1)

applyNtimes :: (Num n, Ord n) => n -> (a -> a) -> a -> a
applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)

isLightOn :: Int -> Int -> Snapper
isLightOn snapperCount snapsTimes =
  head $
  applyNtimes snapsTimes (trace1 snap) $
  initialSnapperChain snapperCount

initialSnapperChain :: Int -> [Snapper]
initialSnapperChain snapperCount =
  reverse $ ON : replicate (snapperCount - 1) OFF

snap :: [Snapper] -> [Snapper]
snap [] = []
snap [x] =
  case x of
    ON -> [OFF]
    OFF -> [ON]
snap (x:y:rest) =
  case (x, y) of
    (ON, OFF) -> ON : snap (y:rest)
    (OFF, OFF) -> OFF : snap (y:rest)
    (ON, ON) -> OFF : snap (y:rest)
    (OFF, ON) -> ON : snap (y:rest)

-- IO

extractDevicesAndSnaps :: String -> (Snappers, Snaps)
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
  map (uncurry isLightOn) $
  map extractDevicesAndSnaps $
  caseLines

main :: IO()
main = do
  inputLines <- lines <$> getContents
  putStr $ unlines $ snapperCLI $ drop 1 inputLines