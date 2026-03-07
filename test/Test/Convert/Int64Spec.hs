module Test.Convert.Int64Spec (spec) where

import Test.Hspec
import Data.Int
import qualified Unwitch.Convert.Int64 as Int64

-- Property tests cover: toInt32 narrowing, toInteger widening,
-- toWord64 (same-width), toNatural iff negative,
-- toFloat range check, toDouble range check.
-- Kept: toInt8/toInt16 (no property), toInt (no property),
-- cross-width signed-to-unsigned.

spec :: Spec
spec = describe "Unwitch.Convert.Int64" $ do

  describe "toInt8 (fallible)" $ do
    it "narrows in-range" $
      Int64.toInt8 (42 :: Int64) `shouldBe` Just (42 :: Int8)
    it "rejects out-of-range" $
      Int64.toInt8 (200 :: Int64) `shouldBe` Nothing

  describe "toInt16 (fallible)" $
    it "rejects out-of-range" $
      Int64.toInt16 (40000 :: Int64) `shouldBe` Nothing

  describe "toInt (fallible)" $
    it "converts 0" $
      Int64.toInt 0 `shouldBe` Just 0

  describe "toWord8 (fallible)" $
    it "rejects negative" $
      Int64.toWord8 (-1 :: Int64) `shouldBe` Nothing

  describe "toWord16 (fallible)" $
    it "rejects negative" $
      Int64.toWord16 (-1 :: Int64) `shouldBe` Nothing

  describe "toWord32 (fallible)" $
    it "rejects negative" $
      Int64.toWord32 (-1 :: Int64) `shouldBe` Nothing

  describe "toWord (fallible)" $
    it "rejects negative" $
      Int64.toWord (-1 :: Int64) `shouldBe` Nothing
