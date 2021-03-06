module HaskellProgramming.SemiGroup where

import Test.QuickCheck
import Kata.Utils

--
-- Laws
--

semigroupAssociativity :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssociativity a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

--
-- Examples:
--

-- Opcional (requires SemiGroup on wrapped value)

data Opcional a =
    Nada
  | Valor a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Opcional a) where
  Nada <> Nada = Nada
  Nada <> (Valor x) = Valor x
  (Valor x) <> Nada = Valor x
  (Valor x) <> (Valor y) = Valor (x <> y)
  
instance Arbitrary a => Arbitrary (Opcional a) where
  arbitrary = frequency [ (1, Valor <$> arbitrary) , (1, return Nada) ]

-- Trivial

data Trivial = 
  Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- Identity

newtype Identity a = 
  Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-- Two

data Two a b = 
  Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two x y <> Two x' y' = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = uncurry Two <$> ((,) <$> arbitrary <*> arbitrary)

-- Three

data Three a b c = 
  Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = uncurry3 Three <$>
    ((,,) <$> arbitrary <*> arbitrary <*> arbitrary)

-- Four

data Four a b c d = 
  Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four a b c d <> Four a' b' c' d' = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = uncurry4 Four <$>
    ((,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)

-- BoolConj (and)

newtype BoolConj = 
  BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  BoolConj True <> BoolConj False = BoolConj False
  BoolConj False <> BoolConj True = BoolConj False
  BoolConj False <> BoolConj False = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

-- BoolDisj (or)

newtype BoolDisj = 
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj True <> BoolDisj True = BoolDisj True
  BoolDisj True <> BoolDisj False = BoolDisj True
  BoolDisj False <> BoolDisj True = BoolDisj True
  BoolDisj False <> BoolDisj False = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

-- Special 'Or'

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd x <> _ = Snd x
  _ <> Snd y = Snd y
  Fst _ <> Fst y = Fst y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = frequency [ (1, Fst <$> arbitrary) , (1, Snd <$> arbitrary) ]

-- Combine

newtype Combine a b = Combine { unCombine :: a -> b }  
  
--instance (Arbitrary a, Eq b) => Eq (Combine a b) where
--  c == c' = unCombine c == unCombine c' 
--  
--instance (Eq a, Eq b) => Eq (a -> b) where
--  f == g = $ do
--     x <- (arbitrary :: Gen a)
--     y <- (arbitrary :: Gen b)
--     return $ f x == g y 
--  
--instance Show (Combine a b) where
--  show = const "->"

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \x -> f x <> g x
  
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = Combine <$> arbitrary

-- Compose

newtype Compose a = Compose { unCompose :: a -> a }

instance Semigroup (Compose a) where
  Compose f <> Compose g = Compose (f . g)

-- Validation

data Validation a b =
    Fail a 
  | Value b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Fail x <> Fail y = Fail (x <> y)
  Fail x <> Value _ = Fail x
  Value _ <> Fail y = Fail y
  Value _ <> Value y = Value y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = frequency [ (1, Fail <$> arbitrary) , (1, Value <$> arbitrary) ]
  
-- AccumulateRight

newtype AccumulateRight a b = 
  AccumulateRight (Validation a b) 
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Fail _)  <> AccumulateRight (Fail y)  = AccumulateRight (Fail y)
  AccumulateRight (Fail x)  <> AccumulateRight (Value _) = AccumulateRight (Fail x)
  AccumulateRight (Value _) <> AccumulateRight (Fail y)  = AccumulateRight (Fail y)
  AccumulateRight (Value x) <> AccumulateRight (Value y) = AccumulateRight (Value (x <> y))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = AccumulateRight <$> arbitrary

-- AccumulateBoth

newtype AccumulateBoth a b = 
  AccumulateBoth (Validation a b) 
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Fail x)  <> AccumulateBoth (Fail y)  = AccumulateBoth (Fail (x <> y))
  AccumulateBoth (Fail x)  <> AccumulateBoth (Value _) = AccumulateBoth (Fail x)
  AccumulateBoth (Value _) <> AccumulateBoth (Fail y)  = AccumulateBoth (Fail y)
  AccumulateBoth (Value x) <> AccumulateBoth (Value y) = AccumulateBoth (Value (x <> y))

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = AccumulateBoth <$> arbitrary
  
-- Mem

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance (Semigroup a) => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem $ \x ->
    let r = f x
        r' = g (snd r)
    in (fst r <> fst r', snd r')
    
--instance (CoArbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
--  arbitrary = Mem <$> arbitrary
--  
--instance (CoArbitrary s, Arbitrary a) => Arbitrary (s -> (a, s)) where
--  arbitrary = do 
--    f <- (arbitrary :: Gen (s -> s))
--    y <- (arbitrary :: Gen a)
--    return (\s' -> (y, f s'))