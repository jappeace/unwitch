module Test.Convert.IntegerSpec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import Unwitch.Errors
import qualified Unwitch.Convert.Integer as Integer

spec :: Spec
spec = describe "Unwitch.Convert.Integer" $ do

  describe "toDouble (range-checked)" $ do
    it "converts in-range" $
      Integer.toDouble 9007199254740991 `shouldBe` Right 9007199254740991.0
    it "rejects too large" $
      Integer.toDouble 9007199254740992 `shouldBe` Left Overflow
    it "rejects too small" $
      Integer.toDouble (-9007199254740992) `shouldBe` Left Underflow
    it "converts 0" $
      Integer.toDouble 0 `shouldBe` Right 0.0

  describe "toFloat (range-checked)" $ do
    it "converts in-range" $
      Integer.toFloat 16777215 `shouldBe` Right 16777215.0
    it "rejects too large" $
      Integer.toFloat 16777216 `shouldBe` Left Overflow
    it "rejects too small" $
      Integer.toFloat (-16777216) `shouldBe` Left Underflow
    it "converts 0" $
      Integer.toFloat 0 `shouldBe` Right 0.0

  describe "toNatural" $ do
    it "rejects negative" $
      Integer.toNatural (-1) `shouldBe` Left Underflow
    it "converts 0" $
      Integer.toNatural 0 `shouldBe` Right (0 :: Natural)
    it "converts positive" $
      Integer.toNatural 42 `shouldBe` Right (42 :: Natural)

  describe "toInt8 (fallible)" $ do
    it "converts in-range" $
      Integer.toInt8 42 `shouldBe` Just (42 :: Int8)
    it "rejects too large" $
      Integer.toInt8 128 `shouldBe` Nothing
    it "rejects too small" $
      Integer.toInt8 (-129) `shouldBe` Nothing

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

  describe "toWord8 (fallible)" $ do
    it "rejects negative" $
      Integer.toWord8 (-1) `shouldBe` Nothing
    it "rejects too large" $
      Integer.toWord8 256 `shouldBe` Nothing

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
