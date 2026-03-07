module Test.Convert.RatioSpec (spec) where

import Test.Hspec
import Data.Ratio ((%))
import qualified Unwitch.Convert.Ratio as Ratio

spec :: Spec
spec = describe "Unwitch.Convert.Ratio" $ do

  describe "fromIntegralToRatio" $ do
    it "wraps integer as ratio with denominator 1" $
      Ratio.fromIntegralToRatio (5 :: Integer) `shouldBe` (5 % 1)
    it "wraps zero" $
      Ratio.fromIntegralToRatio (0 :: Integer) `shouldBe` (0 % 1)
    it "wraps negative" $
      Ratio.fromIntegralToRatio ((-3) :: Integer) `shouldBe` ((-3) % 1)

  describe "unwrapIfDenominatorOne" $ do
    it "round-trips with fromIntegralToRatio" $
      Ratio.unwrapIfDenominatorOne (Ratio.fromIntegralToRatio (7 :: Integer))
        `shouldBe` Just 7
    it "rejects non-unit denominator" $
      Ratio.unwrapIfDenominatorOne (3 % 2 :: Rational) `shouldBe` Nothing

  describe "toFloat" $ do
    it "converts simple rational" $
      Ratio.toFloat (3 % 2) `shouldBe` (1.5 :: Float)
    it "converts integer rational" $
      Ratio.toFloat (4 % 1) `shouldBe` (4.0 :: Float)

  describe "toDouble" $ do
    it "converts simple rational" $
      Ratio.toDouble (3 % 2) `shouldBe` (1.5 :: Double)
    it "converts integer rational" $
      Ratio.toDouble (4 % 1) `shouldBe` (4.0 :: Double)
