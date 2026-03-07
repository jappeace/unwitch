module Test.Convert.Word32Spec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import Unwitch.Errors
import qualified Unwitch.Convert.Word32 as Word32

spec :: Spec
spec = describe "Unwitch.Convert.Word32" $ do

  describe "toWord8 (fallible)" $
    it "rejects too large" $
      Word32.toWord8 (256 :: Word32) `shouldBe` Nothing

  describe "toWord16 (fallible)" $
    it "rejects too large" $
      Word32.toWord16 (65536 :: Word32) `shouldBe` Nothing

  describe "toWord64 (infallible)" $
    it "widens maxBound" $
      Word32.toWord64 maxBound `shouldBe` 4294967295

  describe "toWord (fallible)" $
    it "converts 0" $
      Word32.toWord 0 `shouldBe` Just (0 :: Word)

  describe "toNatural (infallible)" $
    it "converts maxBound" $
      Word32.toNatural maxBound `shouldBe` (4294967295 :: Natural)

  describe "toInt8 (fallible)" $
    it "rejects too large" $
      Word32.toInt8 (200 :: Word32) `shouldBe` Nothing

  describe "toInt16 (fallible)" $
    it "rejects too large" $
      Word32.toInt16 (40000 :: Word32) `shouldBe` Nothing

  describe "toInt32 (fallible)" $ do
    it "rejects too large" $
      Word32.toInt32 (2147483648 :: Word32) `shouldBe` Nothing
    it "converts in-range" $
      Word32.toInt32 (2147483647 :: Word32) `shouldBe` Just (2147483647 :: Int32)

  describe "toInt64 (infallible)" $
    it "widens maxBound" $
      Word32.toInt64 maxBound `shouldBe` 4294967295

  describe "toInt (fallible)" $
    it "converts 0" $
      Word32.toInt 0 `shouldBe` Just (0 :: Int)

  describe "toInteger (infallible)" $
    it "converts maxBound" $
      Word32.toInteger maxBound `shouldBe` 4294967295

  describe "toFloat (range-checked)" $ do
    it "converts in-range" $
      Word32.toFloat (16777215 :: Word32) `shouldBe` Right 16777215.0
    it "rejects too large" $
      Word32.toFloat (16777216 :: Word32) `shouldBe` Left Overflow

  describe "toDouble (infallible)" $
    it "converts maxBound" $
      Word32.toDouble maxBound `shouldBe` 4294967295.0
