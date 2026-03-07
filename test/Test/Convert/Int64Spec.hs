module Test.Convert.Int64Spec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import Unwitch.Errors
import qualified Unwitch.Convert.Int64 as Int64

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

  describe "toInt32 (fallible)" $
    it "rejects out-of-range" $
      Int64.toInt32 (3000000000 :: Int64) `shouldBe` Nothing

  describe "toInt (fallible)" $
    it "converts 0" $
      Int64.toInt 0 `shouldBe` Just 0

  describe "toInteger (infallible)" $
    it "converts maxBound" $
      Int64.toInteger maxBound `shouldBe` 9223372036854775807

  describe "toWord8 (fallible)" $
    it "rejects negative" $
      Int64.toWord8 (-1 :: Int64) `shouldBe` Nothing

  describe "toWord16 (fallible)" $
    it "rejects negative" $
      Int64.toWord16 (-1 :: Int64) `shouldBe` Nothing

  describe "toWord32 (fallible)" $
    it "rejects negative" $
      Int64.toWord32 (-1 :: Int64) `shouldBe` Nothing

  describe "toWord64 (fallible)" $ do
    it "rejects negative" $
      Int64.toWord64 (-1 :: Int64) `shouldBe` Nothing
    it "converts maxBound" $
      Int64.toWord64 maxBound `shouldBe` Just (9223372036854775807 :: Word64)

  describe "toWord (fallible)" $
    it "rejects negative" $
      Int64.toWord (-1 :: Int64) `shouldBe` Nothing

  describe "toNatural" $ do
    it "rejects negative" $
      Int64.toNatural (-1 :: Int64) `shouldBe` Left Underflow
    it "converts 0" $
      Int64.toNatural 0 `shouldBe` Right (0 :: Natural)

  describe "toFloat (range-checked)" $ do
    it "converts in-range" $
      Int64.toFloat (16777215 :: Int64) `shouldBe` Right 16777215.0
    it "rejects too large" $
      Int64.toFloat (16777216 :: Int64) `shouldBe` Left Overflow
    it "rejects too small" $
      Int64.toFloat (-16777216 :: Int64) `shouldBe` Left Underflow

  describe "toDouble (range-checked)" $ do
    it "converts in-range" $
      Int64.toDouble (9007199254740991 :: Int64) `shouldBe` Right 9007199254740991.0
    it "rejects too large" $
      Int64.toDouble (9007199254740992 :: Int64) `shouldBe` Left Overflow
    it "rejects too small" $
      Int64.toDouble (-9007199254740992 :: Int64) `shouldBe` Left Underflow
