module Kata.FourThought where

import Data.List

type BinaryOp = Int -> Int -> Int
type Op4 = Int -> Int
type Solution = ([Char], Int)

opers :: [BinaryOp]
opers = [(+), (-), (*), quot]

opersTags = ['+', '-', '*', '/']

opers4 :: [Op4]
opers4 = map (\x -> flip x 4) opers -- flip to use fixed 4 value as second operand

taggedOpers4 :: [(Char, Op4)]
taggedOpers4 = zip opersTags opers4

allSolutions :: [Solution]
allSolutions = [ ([sym1,sym2,sym3], calculate [op1,op2,op3]) | (sym1, op1) <- taggedOpers4,
                                                               (sym2, op2) <- taggedOpers4,
                                                               (sym3, op3) <- taggedOpers4]

calculate :: [Op4] -> Int
calculate ls = aggregateOpers ls 4

aggregateOpers :: [Op4] -> Op4
aggregateOpers = foldr (flip (.)) id -- flip to fold operands in left-right order

findSolution :: Int -> Maybe Solution
findSolution n = find (\el -> snd el == n) allSolutions


-- IO

solutionLiteral :: Maybe Solution -> String
solutionLiteral (Just (symbols, result)) = let opSequence = "4 " ++ unwords (map (: " 4") symbols)
                                           in intercalate " = " [opSequence, show result]
solutionLiteral _                        = "no solution"

main :: IO()
main = do
    target <- getLine
    let x = (read target :: Int)
    putStrLn $ solutionLiteral $ findSolution x