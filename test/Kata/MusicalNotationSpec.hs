module Kata.MusicalNotationSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Kata.MusicalNotation as SUT


spec :: Spec
spec = do

  describe "Notes to line" $ do

    it "Sample 1" $ do
      SUT.musicalNotationCLI 
        "C C D E C E D2 C C D E C2 B2 C C D E F E D C B g A B C2 C2"   
      `shouldBe` "G:                                                           \n\
                 \F: -------------------------------------*--------------------\n\
                 \E:       *   *          *             *   *                  \n\
                 \D: ----*-------**-----*-------------*-------*----------------\n\
                 \C: * *     *      * *     **    * *           *         ** **\n\
                 \B: --------------------------**-----------------*-----*------\n\
                 \A:                                                  *        \n\
                 \g: -----------------------------------------------*----------\n\
                 \f:                                                           \n\
                 \e: ----------------------------------------------------------\n\
                 \d:                                                           \n\
                 \c:                                                           \n\
                 \b:                                                           \n\
                 \a: ----------------------------------------------------------"
                 
    it "Sample 2" $ do
      SUT.musicalNotationCLI 
        "e2 e d c2 c2 d2 d f e d c2 g2 g f e2 e2 e d c d e c3"   
      `shouldBe`  "G:                                                      \n\
                  \F: -----------------------------------------------------\n\
                  \E:                                                      \n\
                  \D: -----------------------------------------------------\n\
                  \C:                                                      \n\
                  \B: -----------------------------------------------------\n\
                  \A:                                                      \n\
                  \g: ---------------------------**-*----------------------\n\
                  \f:                   *             *                    \n\
                  \e: **-*----------------*-------------**-**-*-------*----\n\
                  \d:      *       ** *     *                   *   *      \n\
                  \c:        ** **            **                  *     ***\n\
                  \b:                                                      \n\
                  \a: -----------------------------------------------------"

 