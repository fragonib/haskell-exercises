module TimeMachine.StoreRecordsSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import TimeMachine.StoreRecords

spec :: Spec
spec = do

    it "when input is empty" $
      yearDiscount [] `shouldBe` []

    it "when there are time several machines" $
      let actual = yearDiscount [ TimeMachine { manufacturer = "ACME", model = 1, name = "OClock", timeTravel = Past, price = 1500.0 } ,
                                  TimeMachine { manufacturer = "ACME", model = 2, name = "OClock 2", timeTravel = Both, price = 1500.0 } ]
      in actual `shouldBe` [ TimeMachine { manufacturer = "ACME", model = 1, name = "OClock", timeTravel = Past, price = 750.0 } ,
                             TimeMachine { manufacturer = "ACME", model = 2, name = "OClock 2", timeTravel = Both, price = 750.0 } ]

