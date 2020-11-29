{-# LANGUAGE DeriveFunctor #-}
module Elephant.Functors.Tree where

data Tree a = 
  Empty | 
  Node a (Tree a) (Tree a) 
  deriving (Show, Ord, Eq)

insert :: Ord a => a -> Tree a -> Tree a
insert a Empty = Node a Empty Empty
insert a (Node a' l r) | a <= a' = Node a' (insert a l) r
                       | otherwise = Node a' l (insert a r)

level :: Int -> Tree a -> [a]
level _ Empty = []
level 0 (Node a _ _) = [a]
level n (Node _ l r) = (level (n-1) l) ++ (level (n-1) r)

lefts :: Tree a -> [a]
lefts Empty = []
lefts (Node a l _) = a : lefts l

superTree :: Int -> Tree Int
superTree n = let child = superTree $ n + 1
              in Node n child child 

instance Functor Tree where
   -- fmap :: (a->b) -> Tree a -> Tree b
   fmap f Empty = Empty
   fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

main :: IO ()
main = do
         let t = superTree 0
         print $ level 3 t
         print $ level 3 (fmap (*2) t)
         print $ take 10 $ lefts t
         putStr $ unlines $ take 10 $ lefts $ fmap (flip replicate '*') t  
        -- print $ insert Empty (fmap (flip insert Empty) t)
