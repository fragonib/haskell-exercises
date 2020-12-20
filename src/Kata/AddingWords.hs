module Kata.AddingWords where

import Control.Monad.State (MonadState (get, put), State, evalState)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe (fromMaybe)


type Variables = M.Map String Int
type Values = IM.IntMap String
type AddWordsM = State (Variables, Values)

solve :: [[String]] -> [String]
solve xs = evalState (solveM xs) (M.empty, IM.empty)

solveM :: [[String]] -> AddWordsM [String]
solveM [] = return []
solveM (["def", var, value]:xs) = storeVariable var (read value) >> solveM xs
solveM (("calc":args):xs) = (:) <$> compute args <*> solveM xs
solveM (["clear"]:xs) = clearState >> solveM xs

compute :: [String] -> AddWordsM String
compute args = return "unknown"

storeVariable :: String -> Int -> AddWordsM ()
storeVariable variable value = do
  (variablesMap, valuesMap) <- get
  put $ case M.lookup variable variablesMap of
    Just oldValue -> (
        M.insert variable value variablesMap, 
        IM.insert value variable $ IM.delete oldValue valuesMap
        )
    Nothing -> (
        M.insert variable value variablesMap, 
        IM.insert value variable valuesMap
        )

getValueName :: Int -> AddWordsM String
getValueName value = do
  (_, valuesMap) <- get
  return $ fromMaybe "unknown" $ IM.lookup value valuesMap

readVariable :: String -> AddWordsM (Maybe Int)
readVariable variable = do
  (variablesMap, _) <- get
  return $ M.lookup variable variablesMap

clearState :: AddWordsM ()
clearState = put (M.empty, IM.empty)


-- IO

main :: IO ()
main = interact $ writeOutput . solve . readInput

writeOutput :: [String] -> String
writeOutput = unlines

readInput :: String -> [[String]]
readInput = map words . lines