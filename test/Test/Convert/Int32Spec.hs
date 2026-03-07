module Test.Convert.Int32Spec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import Unwitch.Errors
import qualified Unwitch.Convert.Int32 as Int32

spec :: Spec
spec = describe "Unwitch.Convert.Int32" $ do

  describe "toInt8 (fallible)" $ do
    it "narrows in-range" $
      Int32.toInt8 (100 :: Int32) `shouldBe` Just (100 :: Int8)
    it "rejects out-of-range" $
      Int32.toInt8 (200 :: Int32) `shouldBe` Nothing

  describe "toInt16 (fallible)" $ do
    it "narrows in-range" $
      Int32.toInt16 (1000 :: Int32) `shouldBe` Just (1000 :: Int16)
    it "rejects out-of-range" $
      Int32.toInt16 (40000 :: Int32) `shouldBe` Nothing

  describe "toInt64 (infallible)" $ do
    it "widens minBound" $
      Int32.toInt64 minBound `shouldBe` (-2147483648)
    it "widens maxBound" $
      Int32.toInt64 maxBound `shouldBe` 2147483647

  describe "toInt (fallible via toIntegralSized)" $
    it "converts 0" $
      Int32.toInt 0 `shouldBe` Just 0

  describe "toInteger (infallible)" $
    it "converts maxBound" $
      Int32.toInteger maxBound `shouldBe` 2147483647

  describe "toWord8 (fallible)" $ do
    it "rejects negative" $
      Int32.toWord8 (-1 :: Int32) `shouldBe` Nothing
    it "converts in-range" $
      Int32.toWord8 (255 :: Int32) `shouldBe` Just (255 :: Word8)

  describe "toWord16 (fallible)" $
    it "rejects too large" $
      Int32.toWord16 (70000 :: Int32) `shouldBe` Nothing

  describe "toWord32 (fallible)" $ do
    it "rejects negative" $
      Int32.toWord32 (-1 :: Int32) `shouldBe` Nothing
    it "converts maxBound" $
      Int32.toWord32 (2147483647 :: Int32) `shouldBe` Just (2147483647 :: Word32)

  describe "toWord64 (fallible)" $
    it "rejects negative" $
      Int32.toWord64 (-1 :: Int32) `shouldBe` Nothing

  describe "toWord (fallible)" $
    it "rejects negative" $
      Int32.toWord (-1 :: Int32) `shouldBe` Nothing

  describe "toNatural" $ do
    it "rejects negative" $
      Int32.toNatural (-1 :: Int32) `shouldBe` Left Underflow
    it "converts 0" $
      Int32.toNatural 0 `shouldBe` Right (0 :: Natural)
    it "converts maxBound" $
      Int32.toNatural maxBound `shouldBe` Right (2147483647 :: Natural)

  describe "toFloat (range-checked)" $ do
    it "converts in-range value" $
      Int32.toFloat (16777215 :: Int32) `shouldBe` Right 16777215.0
    it "rejects too large" $
      Int32.toFloat (16777216 :: Int32) `shouldBe` Left Overflow
    it "rejects too small" $
      Int32.toFloat (-16777216 :: Int32) `shouldBe` Left Underflow
    it "converts negative in-range" $
      Int32.toFloat (-16777215 :: Int32) `shouldBe` Right (-16777215.0)

  describe "toDouble (infallible)" $ do
    it "converts minBound" $
      Int32.toDouble minBound `shouldBe` (-2147483648.0)
    it "converts maxBound" $
      Int32.toDouble maxBound `shouldBe` 2147483647.0
