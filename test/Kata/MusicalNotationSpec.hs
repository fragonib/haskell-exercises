module Kata.MusicalNotationSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.MusicalNotation as SUT


spec :: Spec
spec = do

  describe "Notes to line" $ do

      it "D line" $ do
        SUT.musicLine 'D' $ SUT.musicalNotationCLI
            "C C D E C E D2 C C D E C2 B2 C C D E F E D C B g A B C2 C2"
        `shouldBe` "D: ----*-------**-----*-------------*-------*----------------"
        
      it "F line" $ do
        SUT.musicLine 'F' $ SUT.musicalNotationCLI
            "C C D E C E D2 C C D E C2 B2 C C D E F E D C B g A B C2 C2"   
        `shouldBe` "F: -------------------------------------*--------------------"
        
        
  
  
   "G:                                                           \
   \F: -------------------------------------*--------------------\
   \E:       *   *          *             *   *                  \
   \D: ----*-------**-----*-------------*-------*----------------\
   \C: * *     *      * *     **    * *           *         ** **\
   \B: --------------------------**-----------------*-----*------\
   \A:                                                  *        \
   \g: -----------------------------------------------*----------\
   \f:                                                           \
   \e: ----------------------------------------------------------\
   \d:                                                           \
   \c:                                                           \
   \b:                                                           \
   \a: ----------------------------------------------------------" 