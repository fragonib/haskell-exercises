#!/usr/bin/env stack
-- stack --install-ghc ghci --resolver lts-16 --package finite-typelits --package groups --package heredoc

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module AdventOfCode.JustinLeeAoC22 where

import           Data.Finite
import           Text.Heredoc
import           Data.Group
import           Data.Proxy
import           Data.Semigroup
import           GHC.TypeNats

data Affine n = Aff
    { aScale :: Finite n
    , aShift :: Finite n
    }

runPerm :: KnownNat n => Affine n -> Finite n -> Finite n
runPerm (Aff a b) x = a * x + b

parseAffine :: KnownNat n => String -> Affine n
parseAffine str = case words str of
    "cut":n:_           -> Aff                1  (-modulo (read n))
    "deal":"into":_     -> Aff        (negate 1)          maxBound
    "deal":"with":_:n:_ -> Aff (modulo (read n))                 0

instance KnownNat n => Semigroup (Affine n) where
    Aff a' b' <> Aff a b = Aff (a' * a) (a' * b + b')

instance KnownNat n => Monoid (Affine n) where
    mempty = Aff 1 0

-- | Group instance only works if n is prime
instance KnownNat n => Group (Affine n) where
    invert (Aff a b) = Aff a' b'
      where
        a' = a ^ (natVal (Proxy @n) - 2)
        b' = negate (a' * b)

-- | Part 1: Given a permutation list, find the place where 2019 ends up
part1 :: [Affine 10007] -> Finite 10007
part1 perms = runPerm bigPerm 2019
  where
    bigPerm = mconcat perms

-- | Part 2: Given a permutation list, find the index that will end up at 2020
part2 :: [Affine 119315717514047] -> Finite 119315717514047
part2 perms = runPerm (invert biiigPerm) 2020
  where
    bigPerm   = mconcat perms
    biiigPerm = stimes 101741582076661 bigPerm

-- | The permutation list for my advent of code account, parsed from
-- `myPuzzleInput`
myShuffles :: KnownNat n => [Affine n]
myShuffles = reverse (map parseAffine myPuzzleInput)

-- | The randomized puzzle input for my advent of code account
myPuzzleInput :: [String]
myPuzzleInput = lines
  [str|deal with increment 73
      |cut -8387
      |deal with increment 41
      |cut 190
      |deal with increment 4
      |cut 6396
      |deal with increment 47
      |cut -9579
      |deal with increment 47
      |cut -1296
      |deal with increment 2
      |cut 3807
      |deal with increment 75
      |cut 8267
      |deal with increment 53
      |cut 5108
      |deal with increment 20
      |cut -62
      |deal with increment 63
      |cut 4435
      |deal into new stack
      |deal with increment 2
      |cut 8436
      |deal with increment 52
      |cut 8420
      |deal with increment 70
      |cut -7602
      |deal with increment 39
      |cut 6737
      |deal into new stack
      |cut -3549
      |deal with increment 63
      |deal into new stack
      |cut -2925
      |deal with increment 59
      |cut -9525
      |deal with increment 12
      |deal into new stack
      |deal with increment 7
      |cut 4619
      |deal with increment 27
      |cut 7141
      |deal with increment 69
      |cut 5221
      |deal with increment 19
      |cut 4288
      |deal into new stack
      |deal with increment 64
      |cut -1618
      |deal with increment 63
      |cut -9384
      |deal with increment 24
      |deal into new stack
      |deal with increment 54
      |cut 429
      |deal into new stack
      |cut 2190
      |deal with increment 28
      |cut -4420
      |deal with increment 10
      |cut 6968
      |deal with increment 34
      |cut 8566
      |deal with increment 4
      |cut 8979
      |deal with increment 58
      |deal into new stack
      |deal with increment 17
      |deal into new stack
      |cut -3775
      |deal with increment 72
      |cut 3378
      |deal with increment 40
      |cut -7813
      |deal into new stack
      |deal with increment 26
      |deal into new stack
      |cut 5504
      |deal with increment 64
      |deal into new stack
      |cut 3592
      |deal with increment 13
      |cut 4123
      |deal into new stack
      |deal with increment 67
      |deal into new stack
      |cut 1943
      |deal with increment 72
      |cut -5205
      |deal into new stack
      |deal with increment 12
      |cut 1597
      |deal with increment 10
      |cut 4721
      |deal with increment 36
      |cut 3379
      |deal into new stack
      |cut -5708
      |deal with increment 61
      |cut 6852
      |]