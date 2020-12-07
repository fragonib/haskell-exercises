module Kata.Monads where

import Control.Monad.Writer
import Data.Monoid (mappend)


-- Dada la función

add :: Monad m => m Int -> (Int -> m Int) -> m Int
add valueM func = do
    a <- valueM
    b <- func a
    return $ a + b

-- Do Notation

-- add :: Maybe String
-- add = Just 3 >>= (\x ->
--       Just 5 >>= (\y ->
--       Just (a + b)))

-- To save us from writing all these annoying lambdas, Haskell gives us do notation.
-- It allows us to write the previous piece of code like this:

-- foo :: Maybe String
-- foo = do
--     x <- Just 3
--     y <- Just 5
--     Just (x + y)

-- Escribir las funciones siguientes de modo que hagan lo mismo que add,
-- pero sin usar ni la notación 'do' ni los operadores '>>' y '>>='

-- Add Maybe

addMaybe :: Maybe Int -> (Int -> Maybe Int) -> Maybe Int
addMaybe Nothing _ = Nothing
addMaybe m@(Just a) f = (+) <$> m <*> f a

-- Add List

addList :: [Int] -> (Int -> [Int]) -> [Int]
addList [] _ = []
addList l f = [a + b | a <- l, b <- f a]

-- Add Writer

type IntWriterM = Writer String Int

intWriter :: Int -> String -> IntWriterM
intWriter value logLine = writer (value, logLine)

addWriter :: IntWriterM -> (Int -> IntWriterM) -> IntWriterM
addWriter initialWriter operation =
  let (value, monoid) = runWriter initialWriter
      (value', monoid') = runWriter $ operation value
  in intWriter (value + value') (monoid `mappend` monoid')

-- addReader :: (Int -> Int) -> (Int -> (Int -> Int)) -> (Int -> Int)

newtype State s a = State { runState :: s -> (a, s) }

-- addState :: State Int Int -> (Int -> State Int Int) -> State Int Int