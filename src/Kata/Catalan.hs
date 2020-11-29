{-# LANGUAGE LambdaCase #-}
module Kata.Catalan where

import Data.IntMap(IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad.State

type CatalanM = State (IntMap Integer)

readInput :: String -> [Int]
readInput = (map read) . words

solve :: [Int] -> [Integer]
solve [] = []
solve l = map (\s -> evalState s IntMap.empty) (map stateCatalan l)

stateCatalan :: Int -> CatalanM Integer
stateCatalan 0 = return 1
stateCatalan 1 = return 1
stateCatalan n =
  recover n >>= \case
    Just r -> return r
    Nothing -> do
      prev <- stateCatalan $ n - 1
      let ni = fromIntegral n 
          r = 2 * ( 2 * ni - 1 ) * prev `div` (ni + 1)
      store n r
      return r

recover :: Int -> CatalanM (Maybe Integer)
recover n = IntMap.lookup n <$> get 

store :: Int -> Integer -> CatalanM ()
store n r = myModify $ IntMap.insert n r

myModify :: (s -> s) -> State s ()
myModify f = get >>= put.f

writeOutput :: [Integer] -> String
writeOutput = unlines . (map show)

io :: String -> String
io = (writeOutput . solve . tail . readInput)

main :: IO ()
main = interact io