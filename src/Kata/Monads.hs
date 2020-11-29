module Kata.Monads where

-- Dada la función

add :: Monad m => m Int -> (Int -> m Int) -> m Int
add prodA prodB = do
    a <- prodA
    b <- prodB a
    return $ a + b

-- Escribir las funciones siguientes de modo que hagan lo mismo que add,
-- pero sin usar ni la notación 'do' ni los operadores '>>' y '>>='

addMaybe :: Maybe Int -> (Int -> Maybe Int) -> Maybe Int
addMaybe Nothing _ = Nothing
addMaybe m@(Just x) f = (+) <$> m <*> f x

addList :: [Int] -> (Int -> [Int]) -> [Int]
addList [] _ = []
addList l f = concatMap (\x -> map (x+) (f x)) l 

-- addWriter :: Writer Int -> (Int -> Writer Int) -> Writer Int

-- addReader :: (Int -> Int) -> (Int -> (Int -> Int)) -> (Int -> Int)

-- addState :: State Int Int -> (Int -> State Int Int) -> State Int Int


-- Asumir las declaraciones siguientes:

newtype Writer w a = Writer (a, w) deriving Show
newtype State s a = State { runState :: s -> (a, s) }
