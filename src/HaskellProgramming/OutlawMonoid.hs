module HaskellProgramming.OutlawMonoid where

import Control.Monad
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Arbitrary


-- This is Type that is BAD Monoid instance, It doesn't enforce Monoid laws

data Bull =
      Fools
    | Twoo
  deriving (Eq, Show)

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools) , (1, return Twoo) ]