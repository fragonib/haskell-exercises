module PracticalHaskell.Chapter3.Origami where

filter :: (a -> Bool) -> [a] -> [a]
filter pred (x:xs) = if pred x
                  then x : PracticalHaskell.Chapter3.Origami.filter pred xs
                  else     PracticalHaskell.Chapter3.Origami.filter pred xs

filterAsFold :: (a -> Bool) -> [a] -> [a]
filterAsFold p = foldr (\x l -> if p x then x : l else l) []

-- Proof filter & filterAsFold are equivalent

-- filterAsFold p [] = foldr (\x l -> if p x then x : l else l) [] []
--                   = [] -- we get back the initial value

-- filterAsFold p (x:xs) = foldr (\x l -> if p x then x : l else l) [] (x:xs)
--                       = (\x l -> if p x then x : l else l)
--                          x (foldr (\x l -> if p x then x : l else l) [] xs)
--                       = if p x
--                         then x : (foldr (\x l -> if p x then x : l else l) [] xs)
--                         else     (foldr (\x l -> if p x then x : l else l) [] xs)
--


map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : PracticalHaskell.Chapter3.Origami.map f xs

mapAsFold :: (a -> b) -> [a] -> [b]
mapAsFold f = foldr (\x l -> f x : l) []

-- Map filter & mapAsFold are equivalent