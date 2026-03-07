module Test.Convert.NaturalSpec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import Unwitch.Errors
import qualified Unwitch.Convert.Natural as Natural

spec :: Spec
spec = describe "Unwitch.Convert.Natural" $ do

  describe "toWord8 (fallible)" $ do
    it "rejects too large" $
      Natural.toWord8 (256 :: Natural) `shouldBe` Nothing
    it "converts in-range" $
      Natural.toWord8 (255 :: Natural) `shouldBe` Just (255 :: Word8)

  describe "toWord16 (fallible)" $
    it "rejects too large" $
      Natural.toWord16 (65536 :: Natural) `shouldBe` Nothing

  describe "toWord32 (fallible)" $
    it "rejects too large" $
      Natural.toWord32 (4294967296 :: Natural) `shouldBe` Nothing

  describe "toWord64 (fallible)" $
    it "rejects too large" $
      Natural.toWord64 (18446744073709551616 :: Natural) `shouldBe` Nothing

  describe "toWord (fallible)" $
    it "converts 0" $
      Natural.toWord 0 `shouldBe` Just (0 :: Word)

  describe "toInt8 (fallible)" $
    it "rejects too large" $
      Natural.toInt8 (128 :: Natural) `shouldBe` Nothing

  describe "toInt16 (fallible)" $
    it "rejects too large" $
      Natural.toInt16 (32768 :: Natural) `shouldBe` Nothing

  describe "toInt32 (fallible)" $
    it "rejects too large" $
      Natural.toInt32 (2147483648 :: Natural) `shouldBe` Nothing

  describe "toInt64 (fallible)" $
    it "converts in-range" $
      Natural.toInt64 (100 :: Natural) `shouldBe` Just (100 :: Int64)

  describe "toInt (fallible)" $
    it "converts 0" $
      Natural.toInt 0 `shouldBe` Just (0 :: Int)

  describe "toInteger (infallible)" $
    it "converts large value" $
      Natural.toInteger (999999999999 :: Natural) `shouldBe` 999999999999

  describe "toFloat (range-checked)" $ do
    it "converts in-range" $
      Natural.toFloat (16777215 :: Natural) `shouldBe` Right 16777215.0
    it "rejects too large" $
      Natural.toFloat (16777216 :: Natural) `shouldBe` Left Overflow

  describe "toDouble (range-checked)" $ do
    it "converts in-range" $
      Natural.toDouble (9007199254740991 :: Natural) `shouldBe` Right 9007199254740991.0
    it "rejects too large" $
      Natural.toDouble (9007199254740992 :: Natural) `shouldBe` Left Overflow
