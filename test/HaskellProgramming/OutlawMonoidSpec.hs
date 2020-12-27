module HaskellProgramming.OutlawMonoidSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import HaskellProgramming.OutlawMonoid
import HaskellProgramming.Monoid (monoidAssociativity, monoidLeftIdentity, monoidRightIdentity)

spec :: Spec
spec = do

  describe "Agnostic monoid" $ do

    it "Associativity" $ do
      quickCheck (monoidAssociativity :: Bull -> Bull -> Bull -> Bool) 
      
    it "Identity" $ do
      quickCheck (monoidLeftIdentity :: Bull -> Bool) 
      quickCheck (monoidRightIdentity :: Bull -> Bool)
