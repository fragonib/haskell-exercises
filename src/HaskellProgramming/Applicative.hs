module HaskellProgramming.Applicative where
  
import Data.List (elemIndex)
import Control.Applicative (liftA2)
import Data.Monoid (Sum(..))

-- Exercise 1 
added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

-- Exercise 2
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer) 
tupled = (,) <$> y <*> z

-- Exercise 3
x3 :: Maybe Int
x3 = elemIndex 3 [1, 2, 3, 4, 5]

y3 :: Maybe Int
y3 = elemIndex 4 [1, 2, 3, 4, 5]

max3 :: Int -> Int -> Int 
max3 = max

maxed :: Maybe Int 
maxed = max3 <$> x3 <*> y3

-- Exercise 4
xs = [1, 2, 3] 
ys = [4, 5, 6]

x4 :: Maybe Integer
x4 = lookup 3 $ zip xs ys

y4 :: Maybe Integer
y4 = lookup 2 $ zip xs ys

summed :: Maybe Integer 
summed = sum <$> liftA2 (,) x4 y4

-- Exercise 5
e5p1 = const <$> Just "Hello" <*> pure "World"
e5p2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- Exercise Variation on Either
data Validation e a = 
    Failure e
  | Success a 
  deriving (Eq, Show)

instance Functor (Validation e) where 
  fmap f (Success a) = Success (f a)
  fmap _ (Failure e) = Failure e
  
instance Monoid e => Applicative (Validation e) where
  pure a = Success a 
  (<*>) (Failure e) (Failure e') = Failure (e <> e') 
  (<*>) (Success f) (Success a) = Success (f a) 
  (<*>) (Failure e) _ = Failure e
  (<*>) _ (Failure e) = Failure e


-- Natural transformation
first :: [a] -> Maybe a
first [] = Nothing
first (x:_) = Just x


-- Applicative instance for Identity.
-- const <$> Identity [1, 2, 3] <*> Identity [9, 9, 9]
newtype Identity a = 
  Identity a 
  deriving (Eq, Ord, Show)

instance Functor Identity where 
  fmap f (Identity a) = Identity (f a) 

instance Applicative Identity where 
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity $ f a

-- Write an Applicative instance for Constant.
-- *Main> Constant (Sum 1) <*> Constant (Sum 2)
-- Constant {getConstant = Sum {getSum = 3}
-- *Main> pure 1 :: Constant String Int
-- Constant {getConstant = ""}
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where 
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where 
  pure _ = Constant mempty
  (<*>) (Constant x) (Constant y) = Constant $ x `mappend` y

--
-- Chapter Exercises
-- NOTE: ':set -XTypeApplications' lets you get info instances with type
--

-- Applicative on []

-- λ> :t (<*>) @[]
-- (<*>) @([]) :: [a -> b] -> [a] -> [b]


-- Applicative on IO

-- λ> :t (<*>) @IO
-- (<*>) @IO :: IO (a -> b) -> IO a -> IO b

-- λ> (++) <$> getLine <*> getLine
-- a
-- b
-- "ab"


-- Applicative on (,) Tuple2

-- λ> :t (<*>) @((,) String)
-- (<*>) @((,) String) :: (String, a -> b) -> (String, a) -> (String, b)

-- λ> (*2) (1,2)
-- (1,4) 

data Tupla a b =
  Tupla a b
  deriving (Show)
 
instance Functor (Tupla a) where
  fmap f (Tupla a b) = Tupla a (f b) 
    
instance Monoid a => Applicative (Tupla a) where
  pure a = Tupla mempty a 
  Tupla a f <*> Tupla a' b = Tupla (a <> a') (f b)


-- Applicative on (->) function

-- pure  :: a -> (e -> a)
-- λ> :t pure @((->) Int)
-- pure @((->) Int) :: a -> Int -> a

-- (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b) 
-- λ> :t (<*>) @((->) Int)
-- (<*>) @((->) Int) :: (Int -> a -> b) -> (Int -> a) -> Int -> b