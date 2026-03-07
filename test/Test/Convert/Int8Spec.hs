module Test.Convert.Int8Spec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import qualified Unwitch.Convert.Int8 as Int8

-- Only cross-width signed-to-unsigned conversions that no property test covers.
-- All other Int8 conversions are fully covered by PropertySpec:
--   round-trips, toNatural iff negative, toFloat/toDouble preserve value,
--   path independence, signed-to-unsigned (same-width Int8->Word8).

spec :: Spec
spec = describe "Unwitch.Convert.Int8" $ do

  describe "toWord16 (fallible)" $ do
    it "rejects negative values" $
      Int8.toWord16 (-1 :: Int8) `shouldBe` Nothing
    it "converts maxBound" $
      Int8.toWord16 (127 :: Int8) `shouldBe` Just (127 :: Word16)

  describe "toWord32 (fallible)" $ do
    it "rejects negative values" $
      Int8.toWord32 (-1 :: Int8) `shouldBe` Nothing
    it "converts maxBound" $
      Int8.toWord32 (127 :: Int8) `shouldBe` Just (127 :: Word32)

  describe "toWord64 (fallible)" $ do
    it "rejects negative values" $
      Int8.toWord64 (-1 :: Int8) `shouldBe` Nothing
    it "converts maxBound" $
      Int8.toWord64 (127 :: Int8) `shouldBe` Just (127 :: Word64)

  describe "toWord (fallible)" $ do
    it "rejects negative values" $
      Int8.toWord (-1 :: Int8) `shouldBe` Nothing
    it "converts maxBound" $
      Int8.toWord (127 :: Int8) `shouldBe` Just (127 :: Word)
