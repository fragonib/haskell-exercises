module Kata.Utils where

import Debug.Trace (trace)

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

trace1 :: (Show a, Show b) => (a -> b) -> (a -> b)
trace1 f x = 
  trace ("f1(x) x=" ++ show x ++ " -> " ++ show result) result
  where result = f x

trace2 :: (Show a, Show b, Show c) => (a -> b -> c) -> (a -> b -> c)
trace2 f x y = 
  trace ("f2(x y) x=" ++ show x ++ " y=" ++ show y ++ " -> " ++ show result) result  
  where result = f x y

reverse :: [a] -> [a]
reverse l = rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)