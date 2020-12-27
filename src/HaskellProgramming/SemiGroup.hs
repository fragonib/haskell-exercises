module HaskellProgramming.SemiGroup where

import Test.QuickCheck

--
-- Laws
--

semigroupAssociativity :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssociativity a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

--
-- Examples:
--

-- Trivial

data Trivial = Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

-- Identity

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Semigroup (Identity a) where
  Identity x <> Identity _ = Identity x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

-- Two

data Two a b = Two a b
  deriving (Eq, Show)

instance Semigroup (Two a b) where
  Two a b <> _ = Two a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = uncurry Two <$> ((,) <$> arbitrary <*> arbitrary)

-- Three

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Semigroup (Three a b c) where
  Three a b c <> _ = Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = uncurry3 Three <$>
    ((,,) <$> arbitrary <*> arbitrary <*> arbitrary)

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c) = f a b c

-- Four

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Semigroup (Four a b c d) where
  Four a b c d <> _ = Four a b c d

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = uncurry4 Four <$>
    ((,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)

uncurry4 :: (a -> b -> c -> d -> e) -> ((a, b, c, d) -> e)
uncurry4 f (a, b, c, d) = f a b c d

-- BoolConj

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  BoolConj True <> BoolConj False = BoolConj False
  BoolConj False <> BoolConj True = BoolConj False
  BoolConj False <> BoolConj False = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

-- BoolDisj

newtype  BoolDisj = BoolDisj Bool
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

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \x -> f x <> g x

-- Comp

newtype Compose a = Compose { unCompose :: a -> a }

instance Semigroup (Compose a) where
  Compose f <> Compose g = Compose (f . g)

-- Validation

data Validation a b =
  Fail a | Value b
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