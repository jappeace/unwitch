module Test.Convert.Word8Spec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import qualified Unwitch.Convert.Word8 as Word8

-- Property tests cover: Word8->Word16/32/64 round-trips,
-- Word8->Int16/32 round-trips, Word8->Integer path independence,
-- toFloat/toDouble preserve value.
-- Kept: toWord (no property), toNatural (no property),
-- toInt8 (no property), toInt64 (no property), toInt (no property).

spec :: Spec
spec = describe "Unwitch.Convert.Word8" $ do

  describe "toWord (infallible)" $
    it "widens maxBound" $
      Word8.toWord maxBound `shouldBe` 255

  describe "toNatural (infallible)" $
    it "converts maxBound" $
      Word8.toNatural maxBound `shouldBe` (255 :: Natural)

  describe "toInt8 (fallible)" $ do
    it "rejects too large" $
      Word8.toInt8 (128 :: Word8) `shouldBe` Nothing
    it "converts in-range" $
      Word8.toInt8 (127 :: Word8) `shouldBe` Just (127 :: Int8)

  describe "toInt64 (infallible)" $
    it "widens maxBound" $
      Word8.toInt64 maxBound `shouldBe` 255

  describe "toInt (infallible)" $
    it "widens maxBound" $
      Word8.toInt maxBound `shouldBe` 255
