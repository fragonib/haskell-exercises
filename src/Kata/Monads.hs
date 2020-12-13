module Kata.Monads where

import Control.Monad.Writer
import Data.Monoid (mappend)

{-
-- Do Notation

addStuff = Just 3 >>= (\x -> Just 5 >>= (\y -> Just (x + y)
addStuff :: Maybe String
addStuff = Just 3 >>= (\x ->
      Just 5 >>= (\y ->
      Just (x + y)

-- To save us from writing all these annoying lambdas, Haskell gives us do notation.
-- It allows us to write the previous piece of code like thi

addStuff :: Maybe String
addStuff = do
    x <- Just 3
    y <- Just 5
    Just (x + y)
-}


-- Dada la función

add :: Monad m => m Int -> (Int -> m Int) -> m Int
add valueM func = do
    a <- valueM
    b <- func a
    return $ a + b

-- Escribir las funciones siguientes de modo que hagan lo mismo que add,
-- pero sin usar ni la notación 'do' ni los operadores '>>' y '>>='

-- Add Maybe

addMaybe :: Maybe Int -> (Int -> Maybe Int) -> Maybe Int
addMaybe Nothing _ = Nothing
addMaybe m@(Just a) f = (+) <$> m <*> f a

-- Add List

addList :: [Int] -> (Int -> [Int]) -> [Int]
addList [] _ = []
addList initialList transformer = [a + b | a <- initialList, b <- transformer a]

-- Add Writer

type IntWriterM = Writer String Int

intWriter :: Int -> String -> IntWriterM
intWriter value logLine = writer (value, logLine)

addWriter :: IntWriterM -> (Int -> IntWriterM) -> IntWriterM
addWriter initialWriter transformer =
  let (initialValue, initialMonoid) = runWriter initialWriter
      (newValue, newMonoid) = runWriter $ transformer initialValue
  in intWriter (initialValue + newValue) (initialMonoid `mappend` newMonoid)

-- Add Reader

type IntFuncM = Int -> Int

addReader :: IntFuncM -> (Int -> IntFuncM) -> IntFuncM
addReader initialFunc transformer =
  \x -> let a = initialFunc x
            b = transformer a x
        in a + b

-- newtype State s a = State { runState :: s -> (a, s) }
-- addState :: State Int Int -> (Int -> State Int Int) -> State Int Int