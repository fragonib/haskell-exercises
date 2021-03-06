module Kata.Monads where

import Control.Monad.State
import Data.Monoid ()

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


-- Ejercicio: Dada la función

add :: Monad m => m Int -> (Int -> m Int) -> m Int
add initialM transformer = do
    a <- initialM
    b <- transformer a
    return $ a + b

-- Escribir las funciones siguientes de modo que hagan lo mismo que add,
-- pero sin usar ni la notación 'do' ni los operadores '>>' y '>>='


-- Solución:

--
-- Add Maybe
--

addMaybe :: Maybe Int -> (Int -> Maybe Int) -> Maybe Int
addMaybe Nothing _ = Nothing
addMaybe (Just value) transformer =
  (value +) <$> transformer value

--
-- Add List
--

addList :: [Int] -> (Int -> [Int]) -> [Int]
addList [] _ = []
addList initialListM transformer =
  [a + b | a <- initialListM, b <- transformer a]

--
-- Add Writer
--

newtype Writer w a = 
  Writer (a, w) 
  deriving (Show, Eq)

type IntWriterM w = Writer w Int

intWriter :: Int -> w -> IntWriterM w
intWriter value logLine = Writer (value, logLine)

runWriter :: Writer w a -> (a, w)
runWriter (Writer t) = t

addWriter :: Monoid w => IntWriterM w -> (Int -> IntWriterM w) -> IntWriterM w
addWriter initialWriterM transformer =
  let (initialValue, initialMonoid) = runWriter initialWriterM
      (newValue, newMonoid) = runWriter $ transformer initialValue
  in intWriter (initialValue + newValue) (initialMonoid <> newMonoid)

--
-- Add Reader
--

type IntFuncM = Int -> Int

addReader :: IntFuncM -> (Int -> IntFuncM) -> IntFuncM
addReader initialFuncM transformer x
  = let
      initialValue = initialFuncM x
      newValue = transformer initialValue x
    in initialValue + newValue

--
-- Add State
--

-- State is defined such:
-- newtype State s a = State { runState :: s -> (a, s) }

type IntStateM = State String Int

addState :: IntStateM -> (Int -> IntStateM) -> IntStateM
addState initialStateM transformer = state $ \s ->
  let (initialValue, initialState) = runState initialStateM s
      (newValue, newState) = runState (transformer initialValue) initialState
  in (initialValue + newValue, newState)
