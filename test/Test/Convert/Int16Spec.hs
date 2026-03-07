module Test.Convert.Int16Spec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import Unwitch.Errors
import qualified Unwitch.Convert.Int16 as Int16

spec :: Spec
spec = describe "Unwitch.Convert.Int16" $ do

  describe "toInt8 (fallible)" $ do
    it "narrows in-range value" $
      Int16.toInt8 (42 :: Int16) `shouldBe` Just (42 :: Int8)
    it "rejects too large" $
      Int16.toInt8 (128 :: Int16) `shouldBe` Nothing
    it "rejects too small" $
      Int16.toInt8 (-129 :: Int16) `shouldBe` Nothing

  describe "toInt32 (infallible)" $ do
    it "widens minBound" $
      Int16.toInt32 minBound `shouldBe` (-32768)
    it "widens maxBound" $
      Int16.toInt32 maxBound `shouldBe` 32767

  describe "toInt64 (infallible)" $
    it "widens maxBound" $
      Int16.toInt64 maxBound `shouldBe` 32767

  describe "toInt (infallible)" $
    it "widens maxBound" $
      Int16.toInt maxBound `shouldBe` 32767

  describe "toInteger (infallible)" $
    it "widens maxBound" $
      Int16.toInteger maxBound `shouldBe` 32767

  describe "toWord8 (fallible)" $ do
    it "rejects negative" $
      Int16.toWord8 (-1 :: Int16) `shouldBe` Nothing
    it "rejects too large" $
      Int16.toWord8 (256 :: Int16) `shouldBe` Nothing
    it "converts in range" $
      Int16.toWord8 (255 :: Int16) `shouldBe` Just (255 :: Word8)

  describe "toWord16 (fallible)" $ do
    it "rejects negative" $
      Int16.toWord16 (-1 :: Int16) `shouldBe` Nothing
    it "converts maxBound" $
      Int16.toWord16 (32767 :: Int16) `shouldBe` Just (32767 :: Word16)

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

  describe "toNatural" $ do
    it "rejects negative" $
      Int16.toNatural (-1 :: Int16) `shouldBe` Left Underflow
    it "converts 0" $
      Int16.toNatural 0 `shouldBe` Right (0 :: Natural)

  describe "toFloat (infallible)" $
    it "converts maxBound" $
      Int16.toFloat maxBound `shouldBe` 32767.0

  describe "toDouble (infallible)" $
    it "converts maxBound" $
      Int16.toDouble maxBound `shouldBe` 32767.0
