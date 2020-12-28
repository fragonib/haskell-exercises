module HaskellProgramming.AgnosticMonoid where

import Test.QuickCheck
import HaskellProgramming.Monoid ()
import HaskellProgramming.SemiGroup (Opcional(..))


-- Type that IS a Monoid instance, but doesn't require wrapped value to be a Monoid

newtype OrLikeMonoid a =
  OrLikeMonoid { getAgnostic :: Opcional a }
  deriving (Eq, Show)

instance Semigroup (OrLikeMonoid a) where
  (<>) (OrLikeMonoid Nada) (OrLikeMonoid Nada) = OrLikeMonoid Nada
  (<>) (OrLikeMonoid Nada) (OrLikeMonoid (Valor x)) = OrLikeMonoid (Valor x)
  (<>) (OrLikeMonoid (Valor x)) (OrLikeMonoid Nada) = OrLikeMonoid (Valor x)
  (<>) (OrLikeMonoid (Valor x)) (OrLikeMonoid (Valor _)) = OrLikeMonoid (Valor x)

instance Monoid (OrLikeMonoid a) where
  mempty = OrLikeMonoid Nada

instance Arbitrary a => Arbitrary (OrLikeMonoid a) where
  arbitrary = frequency [ 
        (1, return $ OrLikeMonoid Nada) 
      , (1, OrLikeMonoid . Valor <$> arbitrary) 
    ]