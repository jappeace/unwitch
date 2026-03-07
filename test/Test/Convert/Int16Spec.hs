module Test.Convert.Int16Spec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import qualified Unwitch.Convert.Int16 as Int16

-- Property tests cover: toInt8 narrowing, toInt32/64 round-trips,
-- toWord16 (same-width), toNatural iff negative, toFloat/toDouble.
-- Kept: toInt (no property), cross-width signed-to-unsigned.

spec :: Spec
spec = describe "Unwitch.Convert.Int16" $ do

  describe "toInt (infallible)" $
    it "widens maxBound" $
      Int16.toInt maxBound `shouldBe` 32767

  describe "toWord8 (fallible)" $ do
    it "rejects negative" $
      Int16.toWord8 (-1 :: Int16) `shouldBe` Nothing
    it "rejects too large" $
      Int16.toWord8 (256 :: Int16) `shouldBe` Nothing
    it "converts in range" $
      Int16.toWord8 (255 :: Int16) `shouldBe` Just (255 :: Word8)

  describe "toWord32 (fallible)" $ do
    it "rejects negative" $
      Int16.toWord32 (-1 :: Int16) `shouldBe` Nothing
    it "converts maxBound" $
      Int16.toWord32 (32767 :: Int16) `shouldBe` Just (32767 :: Word32)

  describe "toWord64 (fallible)" $
    it "rejects negative" $
      Int16.toWord64 (-1 :: Int16) `shouldBe` Nothing

  describe "toWord (fallible)" $
    it "rejects negative" $
      Int16.toWord (-1 :: Int16) `shouldBe` Nothing
