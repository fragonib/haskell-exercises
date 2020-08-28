module MathSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import MyMath

spec :: Spec
spec = do

    it "unzip empty list then return tuple2 of empty firsts & seconds" $
      let actual = MyMath.unzip []
      in actual `shouldBe` ([], [])

    it "unzip several tuple2 then tuple2 of list of firsts & list of seconds" $
      let actual = MyMath.unzip [(1, 2), (3, 4), (5, 6)]
      in actual `shouldBe` ([1, 3, 5], [2, 4, 6])

    it "ackermann" $
      let actual = ackermann (0, 2)
      in actual `shouldBe` 3    
      
    it "ackermann2" $
      let actual = ackermann (2, 0)
      in actual `shouldBe` 3
          
    it "ackermann3" $
      let actual = ackermann (2, 2)
      in actual `shouldBe` 7
      
      
      