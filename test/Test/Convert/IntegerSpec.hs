module Test.Convert.IntegerSpec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import qualified Unwitch.Convert.Integer as Integer

-- Property tests cover: toDouble/toFloat/toNatural range checks,
-- toInt8/toWord8 narrowing (success implies fromIntegral).
-- Kept: toInt16, toInt32, toInt64, toInt, toWord16, toWord32,
-- toWord64, toWord (no direct property coverage).

spec :: Spec
spec = describe "Unwitch.Convert.Integer" $ do

  describe "toInt16 (fallible)" $
    it "rejects too large" $
      Integer.toInt16 32768 `shouldBe` Nothing

  describe "toInt32 (fallible)" $
    it "rejects too large" $
      Integer.toInt32 2147483648 `shouldBe` Nothing

  describe "toInt64 (fallible)" $
    it "converts in-range" $
      Integer.toInt64 42 `shouldBe` Just (42 :: Int64)

  describe "toInt (fallible)" $
    it "converts 0" $
      Integer.toInt 0 `shouldBe` Just (0 :: Int)

  describe "toWord16 (fallible)" $
    it "rejects too large" $
      Integer.toWord16 65536 `shouldBe` Nothing

  describe "toWord32 (fallible)" $
    it "rejects too large" $
      Integer.toWord32 4294967296 `shouldBe` Nothing

  describe "toWord64 (fallible)" $
    it "converts in-range" $
      Integer.toWord64 42 `shouldBe` Just (42 :: Word64)

  describe "toWord (fallible)" $
    it "converts 0" $
      Integer.toWord 0 `shouldBe` Just (0 :: Word)
