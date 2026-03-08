module Test.Convert.DoubleSpec (spec) where

import Test.Hspec
import Data.Fixed (Fixed, E2, E6)
import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import Unwitch.Errors
import qualified Unwitch.Convert.Double as Double

spec :: Spec
spec = describe "Unwitch.Convert.Double" $ do

  describe "toFloat" $
    it "converts 0" $
      Double.toFloat 0.0 `shouldBe` 0.0

  describe "toRational" $ do
    it "converts finite value" $
      Double.toRational 1.5 `shouldBe` Right (3 / 2)
    it "rejects NaN" $
      Double.toRational (0 / 0 :: Double) `shouldSatisfy` isLeft
    it "rejects positive Infinity" $
      Double.toRational (1 / 0 :: Double) `shouldBe` Left (Double.IsInf Overflow)
    it "rejects negative Infinity" $
      Double.toRational ((-1) / 0 :: Double) `shouldBe` Left (Double.IsInf Underflow)

  describe "toInteger" $ do
    it "converts whole number" $
      Double.toInteger 42.0 `shouldBe` Right 42
    it "rejects fractional" $
      Double.toInteger 1.5 `shouldSatisfy` isLeft

  describe "toInt8" $ do
    it "converts in-range" $
      Double.toInt8 42.0 `shouldBe` Right (42 :: Int8)
    it "rejects out-of-range" $
      Double.toInt8 200.0 `shouldSatisfy` isLeft

  describe "toInt16" $
    it "converts in-range" $
      Double.toInt16 1000.0 `shouldBe` Right (1000 :: Int16)

  describe "toInt32" $
    it "converts in-range" $
      Double.toInt32 100000.0 `shouldBe` Right (100000 :: Int32)

  describe "toInt64" $
    it "converts in-range" $
      Double.toInt64 100000.0 `shouldBe` Right (100000 :: Int64)

  describe "toInt" $
    it "converts in-range" $
      Double.toInt 42.0 `shouldBe` Right (42 :: Int)

  describe "toWord8" $ do
    it "converts in-range" $
      Double.toWord8 200.0 `shouldBe` Right (200 :: Word8)
    it "rejects negative" $
      Double.toWord8 (-1.0) `shouldSatisfy` isLeft

  describe "toWord16" $
    it "converts in-range" $
      Double.toWord16 1000.0 `shouldBe` Right (1000 :: Word16)

  describe "toWord32" $
    it "converts in-range" $
      Double.toWord32 100000.0 `shouldBe` Right (100000 :: Word32)

  describe "toWord64" $
    it "converts in-range" $
      Double.toWord64 100000.0 `shouldBe` Right (100000 :: Word64)

  describe "toWord" $
    it "converts in-range" $
      Double.toWord 42.0 `shouldBe` Right (42 :: Word)

  describe "toNatural" $ do
    it "converts positive whole number" $
      Double.toNatural 42.0 `shouldBe` Right (42 :: Natural)
    it "rejects negative" $
      Double.toNatural (-1.0) `shouldSatisfy` isLeft
    it "rejects fractional" $
      Double.toNatural 1.5 `shouldSatisfy` isLeft

  describe "toFixed" $ do
    it "converts finite Double to Fixed" $
      Double.toFixed 1.5 `shouldBe` Right (1.50 :: Fixed E2)
    it "converts zero" $
      Double.toFixed 0.0 `shouldBe` Right (0.0 :: Fixed E6)
    it "rejects NaN" $
      Double.toFixed (0 / 0 :: Double) `shouldSatisfy` (isLeft :: Either Double.RationalErrors (Fixed E2) -> Bool)
    it "rejects Infinity" $
      Double.toFixed (1 / 0 :: Double) `shouldBe` (Left (Double.IsInf Overflow) :: Either Double.RationalErrors (Fixed E2))

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
