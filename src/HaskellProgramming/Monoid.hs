module HaskellProgramming.Monoid where

import Data.Monoid


-- Laws

associativity :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associativity (<>) a b c =
  a <> (b <> c) == (a <> b) <> c

monoidAssociativity :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssociativity a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = (mempty <> x) == x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = (x <> mempty) == x


-- Example that requires Monoid on wrapped value

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



--

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' excl adv noun adj =
  mconcat [
    excl, "! he said ",
    adv, " as he jumped into his car ", 
    noun, " and drove off with his ", 
    adj, " wife."
  ]

