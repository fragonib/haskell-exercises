{-# LANGUAGE NamedFieldPuns #-}
module Kata.FourThought where

import Data.List


-- Operators

type BinaryOp = Int -> Int -> Int
type Op4 = Int -> Int

operators :: [BinaryOp]
operators = [(+), (-), (*), quot]

opersTags :: [Char]
opersTags = ['+', '-', '*', '/']

opers4 :: [Op4]
opers4 = map (`flip` 4) operators -- flip to use fixed 4 value as second operand

taggedOpers4 :: [(Char, Op4)]
taggedOpers4 = zip opersTags opers4


-- Solutions

data Solution =
  Solution { opSequence :: [Char],
             result :: Int }
  deriving (Show, Eq)

allSolutions :: [Solution]
allSolutions = [
  Solution { opSequence = [sym1,sym2,sym3], 
             result = calculate [op1,op2,op3] } | 
  (sym1, op1) <- taggedOpers4,
  (sym2, op2) <- taggedOpers4,
  (sym3, op3) <- taggedOpers4 ]

calculate :: [Op4] -> Int
calculate opers = foldr (flip (.)) id opers 4 -- flip to fold operands in left-right order

findSolution :: Int -> Maybe Solution
findSolution n = find (\solution -> result solution == n) allSolutions


-- IO

solutionLiteral :: Maybe Solution -> String
solutionLiteral (Just Solution { opSequence, result }) = 
  intercalate " = " [leftSide, rightSide]
  where leftSide = "4 " ++ unwords (map (: " 4") opSequence)
        rightSide = show result
solutionLiteral _ = "no solution"

main :: IO()
main = do
    readLine <- getLine
    let readInt = (read readLine :: Int)
    putStrLn $ solutionLiteral $ findSolution readInt