module Kata.TriTiling where

--   f(n)   =  f(n-2)   +  g(n-1)  +  g(n-1)
-- ********   AA*******   AA******   A*******
-- ******** = BB******* + B******* + A*******
-- ********   CC*******   B*******   BB******

--   g(n)   =   f(n-1)  +   g(n-2)
-- ********   A********   AA*******
-- ******** = A******** + BB*******
--  *******    ********    CC******

f :: Int -> Int
f 0 = 1
f 1 = 0
f n = f (n - 2) + g (n - 1) + g (n - 1)

g :: Int -> Int
g 0 = 0
g 1 = 1
g n = f (n - 1) + g (n - 2)

triTiling :: Int -> Int
triTiling = f


main :: IO()
main = do
  inputLines <- lines <$> getContents
  let leadingLines = takeWhile (/= "-1") inputLines
  let outputLines = map (show . triTiling . read) leadingLines
   in putStr $ unlines outputLines
