module HaskellProgramming.Chapter15 where

import Data.Monoid


associativity :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associativity (<>) a b c =
  a <> (b <> c) == (a <> b) <> c

monoidAssociativity :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssociativity a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a



data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)


instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada Nada = Nada
  (<>) Nada (Only x) = Only x
  (<>) (Only x) Nada = Only x
  (<>) (Only x) (Only y) = Only (x <> y)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada




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

