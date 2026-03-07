module Test.Convert.Int32Spec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import qualified Unwitch.Convert.Int32 as Int32

-- Property tests cover: toInt16 narrowing, toInt64 round-trip,
-- toWord32 (same-width), toNatural iff negative,
-- toFloat range check, toDouble preserve value.
-- Kept: toInt8 (no property), toInt (no property),
-- cross-width signed-to-unsigned.

spec :: Spec
spec = describe "Unwitch.Convert.Int32" $ do

  describe "toInt8 (fallible)" $ do
    it "narrows in-range" $
      Int32.toInt8 (100 :: Int32) `shouldBe` Just (100 :: Int8)
    it "rejects out-of-range" $
      Int32.toInt8 (200 :: Int32) `shouldBe` Nothing

  describe "toInt (fallible via toIntegralSized)" $
    it "converts 0" $
      Int32.toInt 0 `shouldBe` Just 0

  describe "toWord8 (fallible)" $ do
    it "rejects negative" $
      Int32.toWord8 (-1 :: Int32) `shouldBe` Nothing
    it "converts in-range" $
      Int32.toWord8 (255 :: Int32) `shouldBe` Just (255 :: Word8)

  describe "toWord16 (fallible)" $
    it "rejects too large" $
      Int32.toWord16 (70000 :: Int32) `shouldBe` Nothing

  describe "toWord64 (fallible)" $
    it "rejects negative" $
      Int32.toWord64 (-1 :: Int32) `shouldBe` Nothing

  describe "toWord (fallible)" $
    it "rejects negative" $
      Int32.toWord (-1 :: Int32) `shouldBe` Nothing
