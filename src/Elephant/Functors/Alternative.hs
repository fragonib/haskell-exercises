module Elephant.Functors.Alternative where

import Control.Applicative
import Debug.Trace (trace)

alternative1 :: Maybe Int
alternative1 = trace "calculating alternative1" Nothing

alternative2 :: Maybe Int
alternative2 = trace "calculating alternative2" Just 2

alternative3 :: Maybe Int
alternative3 = trace "calculating alternative3" Just 3

fallback :: Maybe Int 
fallback = alternative1 <|> alternative2 <|> alternative3

main :: IO()
main = do
  putStr $ show fallback
