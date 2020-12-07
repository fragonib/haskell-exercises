module Kata.AddingWords where

import Control.Monad.State (MonadState (get, put), State)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Maybe (fromMaybe)


type Variables = M.Map String Int
type Values = IM.IntMap String
type AddWordsM = State (Variables, Values)


solve :: [String] -> [String]
solve xs = xs

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

getName :: Int -> AddWordsM String
getName value = do
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

readInput :: String -> [String]
readInput = lines