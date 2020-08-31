module Chapter3.ParametricTypes where

swapTriple :: (a, b, c) -> (b, c, a)
swapTriple (x,y,z) = (y,z,x)

duplicate :: a -> (a, a)
duplicate x = (x,x)

nothing :: a -> Maybe a -- Preguntar (p -> Maybe a)
nothing _ = Nothing

index :: Num i => [a] -> [(i, a)]
index [] = []
index [x] = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
               in  (n+1,x):indexed

maybeA :: [a] -> Char
maybeA [] = 'a'
