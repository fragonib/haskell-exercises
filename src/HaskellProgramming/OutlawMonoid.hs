module HaskellProgramming.OutlawMonoid where

import Data.Monoid ()
import Test.QuickCheck
import Test.QuickCheck.Arbitrary ()


-- Type that is BAD Monoid instance, it doesn't enforce Monoid laws

data OutlawMonoid =
  A | B
  deriving (Eq, Show)

instance Semigroup OutlawMonoid where
  _ <> _ = A

instance Monoid OutlawMonoid where
  mempty = A

instance Arbitrary OutlawMonoid where
  arbitrary = frequency [ (1, return A) , (1, return B) ]