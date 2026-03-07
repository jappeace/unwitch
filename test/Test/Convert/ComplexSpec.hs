module Test.Convert.ComplexSpec (spec) where

import Test.Hspec
import Data.Complex (Complex((:+)))
import qualified Unwitch.Convert.Complex as Complex

spec :: Spec
spec = describe "Unwitch.Convert.Complex" $ do

  describe "fromReal" $ do
    it "wraps with zero imaginary part" $
      Complex.fromReal (3.0 :: Double) `shouldBe` (3.0 :+ 0.0)
    it "wraps zero" $
      Complex.fromReal (0 :: Int) `shouldBe` (0 :+ 0)

  describe "toReal" $ do
    it "succeeds when imaginary is 0" $
      Complex.toReal (5.0 :+ 0.0 :: Complex Double) `shouldBe` Just 5.0
    it "fails when imaginary is nonzero" $
      Complex.toReal (5.0 :+ 1.0 :: Complex Double) `shouldBe` Nothing

  describe "round-trip" $
    it "toReal . fromReal == Just" $
      Complex.toReal (Complex.fromReal (42.0 :: Double)) `shouldBe` Just 42.0
