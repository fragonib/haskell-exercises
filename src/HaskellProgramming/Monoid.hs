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

-- Opcional (requires Monoid on wrapped value)

instance Monoid a => Monoid (Opcional a) where
  mempty = Nada

-- Trivial

instance Monoid Trivial where
  mempty = Trivial

-- Identity

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

-- Two

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

-- Three

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty

-- Four

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty

-- BoolConj

instance Monoid BoolConj where
  mempty = BoolConj True

-- BoolDisj

instance Monoid BoolDisj where
  mempty = BoolDisj False
  
-- Or

instance (Monoid a) => Monoid (Or a b) where
  mempty = Fst mempty