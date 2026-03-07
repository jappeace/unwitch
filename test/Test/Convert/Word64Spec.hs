module Test.Convert.Word64Spec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import Unwitch.Errors
import qualified Unwitch.Convert.Word64 as Word64

spec :: Spec
spec = describe "Unwitch.Convert.Word64" $ do

  describe "toWord8 (fallible)" $
    it "rejects too large" $
      Word64.toWord8 (256 :: Word64) `shouldBe` Nothing

  describe "toWord16 (fallible)" $
    it "rejects too large" $
      Word64.toWord16 (65536 :: Word64) `shouldBe` Nothing

  describe "toWord32 (fallible)" $
    it "rejects too large" $
      Word64.toWord32 (4294967296 :: Word64) `shouldBe` Nothing

  describe "toWord (fallible)" $
    it "converts 0" $
      Word64.toWord 0 `shouldBe` Just (0 :: Word)

  describe "toNatural (infallible)" $
    it "converts maxBound" $
      Word64.toNatural maxBound `shouldBe` (18446744073709551615 :: Natural)

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
    it "converts in-range" $
      Word64.toInt64 (9223372036854775807 :: Word64) `shouldBe` Just (9223372036854775807 :: Int64)

  describe "toInt (fallible)" $
    it "converts 0" $
      Word64.toInt 0 `shouldBe` Just (0 :: Int)

  describe "toInteger (infallible)" $
    it "converts maxBound" $
      Word64.toInteger maxBound `shouldBe` 18446744073709551615

  describe "toFloat (range-checked)" $ do
    it "converts in-range" $
      Word64.toFloat (16777215 :: Word64) `shouldBe` Right 16777215.0
    it "rejects too large" $
      Word64.toFloat (16777216 :: Word64) `shouldBe` Left Overflow

  describe "toDouble (range-checked)" $ do
    it "converts in-range" $
      Word64.toDouble (9007199254740991 :: Word64) `shouldBe` Right 9007199254740991.0
    it "rejects too large" $
      Word64.toDouble (9007199254740992 :: Word64) `shouldBe` Left Overflow
