module HaskellProgramming.OutlawMonoidSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import HaskellProgramming.OutlawMonoid
import HaskellProgramming.Chapter15 (monoidAssociativity, monoidLeftIdentity, monoidRightIdentity)

spec :: Spec
spec = do

  describe "outlaw monoid" $ do

    it "Associativity" $ do
      quickCheck (monoidAssociativity :: BullMappend) 
      
    it "Identity" $ do
      quickCheck (monoidLeftIdentity :: Bull -> Bool) 
      quickCheck (monoidRightIdentity :: Bull -> Bool)
