module HaskellProgramming.Monoid where

import Data.Monoid ()
import HaskellProgramming.SemiGroup


--
-- Laws (from SemiGroup laws)
--

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = (mempty <> x) == x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = (x <> mempty) == x


--
-- Examples
--

-- Opcional (that requires SemiGroup / Monoid on wrapped value)

data Opcional a =
    Nada
  | Valor a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Opcional a) where
  (<>) Nada Nada = Nada
  (<>) Nada (Valor x) = Valor x
  (<>) (Valor x) Nada = Valor x
  (<>) (Valor x) (Valor y) = Valor (x <> y)

instance Monoid a => Monoid (Opcional a) where
  mempty = Nada
  mappend = (<>)


-- Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)
  
-- Identity

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

-- Two

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)
  
-- Three

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend = (<>)
  
-- Four

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  mappend = (<>)