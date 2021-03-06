module Kata.Utils where

import Debug.Trace (trace)

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.||.) f g a = f a || g a

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(.&&.) f g a = f a && g a

enumerate :: (Num a, Enum a) => [b] -> [(a, b)]
enumerate = enumerateStartingWith 0

enumerateStartingWith :: (Num a, Enum a) => a -> [b] -> [(a, b)]
enumerateStartingWith start = zip [start..]

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
    
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f (a, b, c, d) = f a b c d