module Kata.Utils where

or :: (a -> Bool) -> (a -> Bool) -> a -> Bool
or f g a = f a || g a

enumerate :: (Num a, Enum a) => [b] -> [(a, b)]
enumerate = zip [0..]

leftPadZero :: Int -> [Int] -> [Int]
leftPadZero = leftPad 0

leftPad :: Int -> Int -> [Int] -> [Int]
leftPad padInt desiredLength xs =
  replicate times padInt ++ xs
  where times = max 0 (desiredLength - length xs)
 
rightPadZero :: Int -> [Int] -> [Int]
rightPadZero = rightPad 0 
  
rightPad :: Int -> Int -> [Int] -> [Int]
rightPad padInt desiredLength xs =
  xs ++ replicate times padInt
  where times = max 0 (desiredLength - length xs)

leftTruncateZero :: [Int] -> [Int]
leftTruncateZero = leftTruncate 0

leftTruncate :: Int -> [Int] -> [Int]
leftTruncate _ [] = []
leftTruncate _ [single] = [single]
leftTruncate el l@(first:rest) =
  if first == el then leftTruncate el rest else l