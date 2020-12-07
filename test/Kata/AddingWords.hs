module Kata.AddingWords where

import Control.Monad.State (MonadState (get, put), State)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe (fromMaybe)

type Variables = Map String Int
type Values = IntMap String
type AddWordsM = State (Variables, Values)


solve :: [String] -> [String]
solve xs = xs

storeVariable :: String -> Int -> AddWordsM ()
storeVariable v n = do
  (m, im) <- get
  put $ case M.lookup v m of
    Just n' -> (M.insert v n m, IM.insert n v $ IM.delete n' im)
    Nothing -> (M.insert v n m, IM.insert n v im)

getName :: Int -> AddWordsM String
getName n = do
  (_, im) <- get
  return $ fromMaybe "unknown" (IM.lookup n im)

readVariable :: String -> AddWordsM (Maybe Int)
readVariable v = do
  (m, _) <- get
  return $ M.lookup v m

clear :: AddWordsM ()
clear = put (M.empty, IM.empty)


-- IO

main :: IO ()
main = interact $ writeOutput . solve . readInput

writeOutput :: [String] -> String
writeOutput = unlines

readInput :: String -> [String]
readInput = lines