module Test.Convert.FixedSpec (spec) where

import Test.Hspec
import Data.Fixed (Fixed, E2, E6)
import Data.Ratio (numerator)
import qualified Unwitch.Convert.Fixed as Fixed

spec :: Spec
spec = describe "Unwitch.Convert.Fixed" $ do

  describe "fromInteger" $ do
    it "converts integer to Fixed E2" $
      Fixed.fromInteger 42 `shouldBe` (42.0 :: Fixed E2)
    it "converts zero" $
      Fixed.fromInteger 0 `shouldBe` (0.0 :: Fixed E2)

  describe "toInteger" $ do
    it "succeeds for whole Fixed values" $
      Fixed.toInteger (42.0 :: Fixed E2) `shouldBe` Just 42
    it "fails for fractional Fixed values" $
      Fixed.toInteger (1.50 :: Fixed E2) `shouldBe` Nothing

  describe "toRational" $ do
    it "converts Fixed to exact Rational" $
      Fixed.toRational (1.50 :: Fixed E2) `shouldBe` (3 / 2)
    it "round-trips whole values through toInteger" $
      let f = 10.0 :: Fixed E2
      in Fixed.toInteger f `shouldBe` Just (numerator (Fixed.toRational f))

  describe "toFixed" $ do
    it "succeeds for compatible resolutions" $
      Fixed.toFixed (1.50 :: Fixed E2) `shouldBe` Just (1.500000 :: Fixed E6)
    it "fails for incompatible resolutions" $
      -- 0.01 as E2 is representable, but converting to E6 should also work
      -- However 1/3 cannot be represented in either, so let's test a value
      -- that exists in E6 but not in E2:
      -- 1.123 in E6 -> 1.12 in E2 loses precision
      Fixed.toFixed (1.123000 :: Fixed E6) `shouldBe` (Nothing :: Maybe (Fixed E2))
