module TimeMachine.StoreSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import TimeMachine.Store

spec :: Spec
spec = do

    it "when input is empty" $
      yearDiscount [] `shouldBe` []

    it "when there are time several machines" $
      let actual = yearDiscount [ TimeMachine (Manufacturer "ACME") (Model 5) "OClock" Past 1500.0,
                                  TimeMachine (Manufacturer "ACME") (Model 5) "OClock" Past 1500.0 ]
      in actual `shouldBe` [ TimeMachine (Manufacturer "ACME") (Model 5) "OClock" Past 750.0,
                             TimeMachine (Manufacturer "ACME") (Model 5) "OClock" Past 750.0 ]

