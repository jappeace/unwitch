module Test.Convert.Word8Spec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import qualified Unwitch.Convert.Word8 as Word8

spec :: Spec
spec = describe "Unwitch.Convert.Word8" $ do

  describe "toWord16 (infallible)" $
    it "widens maxBound" $
      Word8.toWord16 maxBound `shouldBe` 255

  describe "toWord32 (infallible)" $
    it "widens maxBound" $
      Word8.toWord32 maxBound `shouldBe` 255

  describe "toWord64 (infallible)" $
    it "widens maxBound" $
      Word8.toWord64 maxBound `shouldBe` 255

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
    it "converts 0" $
      Word8.toInt8 0 `shouldBe` Just (0 :: Int8)

  describe "toInt16 (infallible)" $
    it "widens maxBound" $
      Word8.toInt16 maxBound `shouldBe` 255

  describe "toInt32 (infallible)" $
    it "widens maxBound" $
      Word8.toInt32 maxBound `shouldBe` 255

  describe "toInt64 (infallible)" $
    it "widens maxBound" $
      Word8.toInt64 maxBound `shouldBe` 255

  describe "toInt (infallible)" $
    it "widens maxBound" $
      Word8.toInt maxBound `shouldBe` 255

  describe "toInteger (infallible)" $
    it "widens maxBound" $
      Word8.toInteger maxBound `shouldBe` 255

  describe "toFloat (infallible)" $
    it "converts maxBound" $
      Word8.toFloat maxBound `shouldBe` 255.0

  describe "toDouble (infallible)" $
    it "converts maxBound" $
      Word8.toDouble maxBound `shouldBe` 255.0
