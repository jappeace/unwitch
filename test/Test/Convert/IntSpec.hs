module Test.Convert.IntSpec (spec) where

import Test.Hspec
import Data.Int
import Numeric.Natural (Natural)
import Unwitch.Errors
import qualified Unwitch.Convert.Int as Int

-- Property tests only cover Int.toNatural (iff negative).
-- All other Int conversions have no property coverage.

spec :: Spec
spec = describe "Unwitch.Convert.Int" $ do

  describe "toInt8 (fallible)" $ do
    it "narrows in-range" $
      Int.toInt8 (42 :: Int) `shouldBe` Just (42 :: Int8)
    it "rejects out-of-range" $
      Int.toInt8 (200 :: Int) `shouldBe` Nothing

  describe "toInt16 (fallible)" $
    it "rejects out-of-range" $
      Int.toInt16 (40000 :: Int) `shouldBe` Nothing

  describe "toInt32 (fallible)" $
    it "converts in-range" $
      Int.toInt32 (1000 :: Int) `shouldBe` Just (1000 :: Int32)

  describe "toInt64 (infallible)" $
    it "widens maxBound" $
      Int.toInt64 maxBound `shouldBe` fromIntegral (maxBound :: Int)

  describe "toInteger (infallible)" $
    it "converts maxBound" $
      Int.toInteger maxBound `shouldBe` fromIntegral (maxBound :: Int)

  describe "toWord8 (fallible)" $
    it "rejects negative" $
      Int.toWord8 (-1 :: Int) `shouldBe` Nothing

  describe "toWord16 (fallible)" $
    it "rejects negative" $
      Int.toWord16 (-1 :: Int) `shouldBe` Nothing

  describe "toWord32 (fallible)" $
    it "rejects negative" $
      Int.toWord32 (-1 :: Int) `shouldBe` Nothing

  describe "toWord64 (fallible)" $
    it "rejects negative" $
      Int.toWord64 (-1 :: Int) `shouldBe` Nothing

  describe "toWord (fallible)" $ do
    it "rejects negative" $
      Int.toWord (-1 :: Int) `shouldBe` Nothing
    it "converts 0" $
      Int.toWord 0 `shouldBe` Just (0 :: Word)

  describe "toNatural" $ do
    it "rejects negative with Underflow" $
      Int.toNatural (-1 :: Int) `shouldBe` Left Underflow
    it "converts 0" $
      Int.toNatural 0 `shouldBe` Right (0 :: Natural)

  describe "toFloat (range-checked)" $ do
    it "converts at boundary" $
      Int.toFloat (16777215 :: Int) `shouldBe` Right 16777215.0
    it "rejects above boundary" $
      Int.toFloat (16777216 :: Int) `shouldBe` Left Overflow

  describe "toDouble (range-checked)" $
    it "converts 0" $
      Int.toDouble 0 `shouldBe` Right (0.0 :: Double)
