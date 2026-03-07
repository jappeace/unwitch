module Test.Convert.FloatSpec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import Unwitch.Errors
import qualified Unwitch.Convert.Float as Float

spec :: Spec
spec = describe "Unwitch.Convert.Float" $ do

  describe "toDouble (infallible)" $ do
    it "converts 0" $
      Float.toDouble 0.0 `shouldBe` 0.0
    it "converts 1.5" $
      Float.toDouble 1.5 `shouldBe` 1.5

  describe "toRational" $ do
    it "converts finite value" $
      Float.toRational 1.5 `shouldBe` Right (3 / 2)
    it "rejects NaN" $
      Float.toRational (0 / 0 :: Float) `shouldSatisfy` isLeft
    it "rejects positive Infinity" $
      Float.toRational (1 / 0 :: Float) `shouldBe` Left (Float.IsInf Overflow)
    it "rejects negative Infinity" $
      Float.toRational ((-1) / 0 :: Float) `shouldBe` Left (Float.IsInf Underflow)

  describe "toInteger" $ do
    it "converts whole number" $
      Float.toInteger 42.0 `shouldBe` Right 42
    it "rejects fractional" $
      Float.toInteger 1.5 `shouldSatisfy` isLeft
    it "rejects NaN" $
      Float.toInteger (0 / 0 :: Float) `shouldSatisfy` isLeft

  describe "toInt8" $ do
    it "converts in-range" $
      Float.toInt8 42.0 `shouldBe` Right (42 :: Int8)
    it "rejects out-of-range" $
      Float.toInt8 200.0 `shouldSatisfy` isLeft
    it "rejects fractional" $
      Float.toInt8 1.5 `shouldSatisfy` isLeft

  describe "toInt16" $
    it "converts in-range" $
      Float.toInt16 1000.0 `shouldBe` Right (1000 :: Int16)

  describe "toInt32" $
    it "converts in-range" $
      Float.toInt32 100000.0 `shouldBe` Right (100000 :: Int32)

  describe "toInt64" $
    it "converts in-range" $
      Float.toInt64 100000.0 `shouldBe` Right (100000 :: Int64)

  describe "toInt" $
    it "converts in-range" $
      Float.toInt 42.0 `shouldBe` Right (42 :: Int)

  describe "toWord8" $ do
    it "converts in-range" $
      Float.toWord8 200.0 `shouldBe` Right (200 :: Word8)
    it "rejects negative" $
      Float.toWord8 (-1.0) `shouldSatisfy` isLeft

  describe "toWord16" $
    it "converts in-range" $
      Float.toWord16 1000.0 `shouldBe` Right (1000 :: Word16)

  describe "toWord32" $
    it "converts in-range" $
      Float.toWord32 100000.0 `shouldBe` Right (100000 :: Word32)

  describe "toWord64" $
    it "converts in-range" $
      Float.toWord64 100000.0 `shouldBe` Right (100000 :: Word64)

  describe "toWord" $
    it "converts in-range" $
      Float.toWord 42.0 `shouldBe` Right (42 :: Word)

  describe "toNatural" $ do
    it "converts positive whole number" $
      Float.toNatural 42.0 `shouldBe` Right (42 :: Natural)
    it "rejects negative" $
      Float.toNatural (-1.0) `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
