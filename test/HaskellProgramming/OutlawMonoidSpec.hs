module HaskellProgramming.OutlawMonoidSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception ()
import HaskellProgramming.SemiGroup (semigroupAssociativity)
import HaskellProgramming.Monoid (monoidLeftIdentity, monoidRightIdentity)
import HaskellProgramming.OutlawMonoid


spec :: Spec
spec = do

  describe "Outlaw Monoid" $ do

    it "Associativity" $ do
      property (semigroupAssociativity :: OutlawMonoid -> OutlawMonoid -> OutlawMonoid -> Bool)
      
    it "Left Identity (violated)" $ do
      property (monoidLeftIdentity :: OutlawMonoid -> Bool)
    
    it "Rigth Identity (violated)" $ do
      property (monoidRightIdentity :: OutlawMonoid -> Bool)
