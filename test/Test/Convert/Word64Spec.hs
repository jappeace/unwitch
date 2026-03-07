module Test.Convert.Word64Spec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import qualified Unwitch.Convert.Word64 as Word64

-- Property tests cover: Word64->Word32 narrowing,
-- Word64->Natural->Integer round-trip,
-- toFloat range check, toDouble range check.
-- Kept: toWord8, toWord16, toWord, toInt8, toInt16, toInt32,
-- toInt64, toInt, toInteger (all have no direct property coverage).

spec :: Spec
spec = describe "Unwitch.Convert.Word64" $ do

  describe "toWord8 (fallible)" $
    it "rejects too large" $
      Word64.toWord8 (256 :: Word64) `shouldBe` Nothing

  describe "toWord16 (fallible)" $
    it "rejects too large" $
      Word64.toWord16 (65536 :: Word64) `shouldBe` Nothing

  describe "toWord (fallible)" $
    it "converts 0" $
      Word64.toWord 0 `shouldBe` Just (0 :: Word)

  describe "toInt8 (fallible)" $
    it "rejects too large" $
      Word64.toInt8 (200 :: Word64) `shouldBe` Nothing

  describe "toInt16 (fallible)" $
    it "rejects too large" $
      Word64.toInt16 (40000 :: Word64) `shouldBe` Nothing

  describe "toInt32 (fallible)" $
    it "rejects too large" $
      Word64.toInt32 (3000000000 :: Word64) `shouldBe` Nothing

  describe "toInt64 (fallible)" $ do
    it "rejects too large" $
      Word64.toInt64 (maxBound :: Word64) `shouldBe` Nothing
    it "converts at boundary" $
      Word64.toInt64 (9223372036854775807 :: Word64) `shouldBe` Just (9223372036854775807 :: Int64)

  describe "toInt (fallible)" $
    it "converts 0" $
      Word64.toInt 0 `shouldBe` Just (0 :: Int)

  describe "toInteger (infallible)" $
    it "converts maxBound" $
      Word64.toInteger maxBound `shouldBe` 18446744073709551615
