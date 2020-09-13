{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE ParallelListComp #-}

module Chapter3.Comprehension (main) where

import Chapter3.ClientRecords
import Chapter3.Folds (clientName)
import Data.Char
import GHC.Exts


-- Filtering input list
duplicateOdds listOfNumbers = [ 2 * x | x <- listOfNumbers, odd x ]
clientNameOfGovOrgs listOfClients = [ clientName x | x@(GovOrg _) <- listOfClients ]

-- Multiple generators
multipleGenerators = [(x, y, x*y) | x <- [1..4], y <- [1..10]]
dependencyBetweenGenerators = [(x, y) | x <- [0..6], y <- [x..6]]
generatorInputAsLists = [ toUpper c | s <- ["A", "list"], c <- ' ':s ]

-- Qualifiers (second form) ---
vectorModule :: [(Double, Double)] -> [Double]
vectorModule vectors = [ sqrt v | (x,y) <- vectors, let v = x*x + y*y ]

-- Filtering using Guards
dominoes = [(x,y) | x <- [1..6], y <- [1..6], x <= y]

-- Extensions
reverseCH = [x*y | x <- [-1,1,-2], y <- [1,2,3], then reverse]
sortingCH = [x*y | x <- [-1,1,-2], y <- [1,2,3] , then sortWith by x]
groupingCH = [ (the p, m) | x <- [-1,1,-2] , y <- [1,2,3]
                          , let m = x*y
                          , let p = m > 0
                          , then group by p using groupWith ]

-- Parallel lists traversal 
parallelCH = [ x*y | x <- [1,2,3] | y <- [1,2,3] ]


--
-- Demo
--
main = do
  print $ duplicateOdds [1, 2, 3, 4, 5, 6]
  print $ clientNameOfGovOrgs [ Individual { person = Person "Fran" "Gonzalez" Male, boss = True }
                              , Company "ACME" 11111111 (Person "Fran" "Gonzalez" Male) "CTO"
                              , GovOrg "Generalitat catalana"
                              , GovOrg "Ministerio de ciencia" ]
  print multipleGenerators
  print dependencyBetweenGenerators
  print generatorInputAsLists
  -- print vectorModule [(1,2),(3,8)]
  print dominoes
  print reverseCH
  print sortingCH
  print groupingCH
  print parallelCH




