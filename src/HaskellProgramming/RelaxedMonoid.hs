module HaskellProgramming.RelaxedMonoid where

import Test.QuickCheck
import HaskellProgramming.Monoid ()
import HaskellProgramming.SemiGroup (Opcional(..))


-- Type it IS a Semigroup/Monoid instance, but doesn't require wrapped type to be Semigroup/Monoid

newtype LeftBiasedOpcionalConj a =
  LeftBiasedOpcionalConj { unconstrainted :: Opcional a }
  deriving (Eq, Show)

instance Semigroup (LeftBiasedOpcionalConj a) where
  LeftBiasedOpcionalConj Nada <> LeftBiasedOpcionalConj Nada = LeftBiasedOpcionalConj Nada
  LeftBiasedOpcionalConj Nada <> LeftBiasedOpcionalConj (Valor x) = LeftBiasedOpcionalConj (Valor x)
  LeftBiasedOpcionalConj (Valor x) <> LeftBiasedOpcionalConj Nada = LeftBiasedOpcionalConj (Valor x)
  LeftBiasedOpcionalConj (Valor x) <> LeftBiasedOpcionalConj (Valor _) = LeftBiasedOpcionalConj (Valor x)

instance Monoid (LeftBiasedOpcionalConj a) where
  mempty = LeftBiasedOpcionalConj Nada

instance Arbitrary a => Arbitrary (LeftBiasedOpcionalConj a) where
  arbitrary = frequency [ 
        (1, return $ LeftBiasedOpcionalConj Nada) 
      , (1, LeftBiasedOpcionalConj . Valor <$> arbitrary) 
    ]