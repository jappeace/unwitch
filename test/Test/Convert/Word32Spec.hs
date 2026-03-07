module Test.Convert.Word32Spec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import qualified Unwitch.Convert.Word32 as Word32

-- Property tests cover: Word32->Word64 round-trip,
-- Word32->Word16 narrowing, toFloat range check,
-- toDouble preserve value.
-- Kept: toWord8 (no property), toWord, toNatural, toInt8, toInt16,
-- toInt32, toInt64, toInt, toInteger.

spec :: Spec
spec = describe "Unwitch.Convert.Word32" $ do

  describe "toWord8 (fallible)" $
    it "rejects too large" $
      Word32.toWord8 (256 :: Word32) `shouldBe` Nothing

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
    it "converts at boundary" $
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
