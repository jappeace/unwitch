module Test.Convert.Word16Spec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import qualified Unwitch.Convert.Word16 as Word16

spec :: Spec
spec = describe "Unwitch.Convert.Word16" $ do

  describe "toWord8 (fallible)" $ do
    it "rejects too large" $
      Word16.toWord8 (256 :: Word16) `shouldBe` Nothing
    it "converts in-range" $
      Word16.toWord8 (255 :: Word16) `shouldBe` Just (255 :: Word8)

  describe "toWord32 (infallible)" $
    it "widens maxBound" $
      Word16.toWord32 maxBound `shouldBe` 65535

  describe "toWord64 (infallible)" $
    it "widens maxBound" $
      Word16.toWord64 maxBound `shouldBe` 65535

  describe "toWord (infallible)" $
    it "widens maxBound" $
      Word16.toWord maxBound `shouldBe` 65535

  describe "toNatural (infallible)" $
    it "converts maxBound" $
      Word16.toNatural maxBound `shouldBe` (65535 :: Natural)

  describe "toInt8 (fallible)" $
    it "rejects too large" $
      Word16.toInt8 (128 :: Word16) `shouldBe` Nothing

  describe "toInt16 (fallible)" $ do
    it "rejects too large" $
      Word16.toInt16 (32768 :: Word16) `shouldBe` Nothing
    it "converts in-range" $
      Word16.toInt16 (32767 :: Word16) `shouldBe` Just (32767 :: Int16)

  describe "toInt32 (infallible)" $
    it "widens maxBound" $
      Word16.toInt32 maxBound `shouldBe` 65535

  describe "toInt64 (infallible)" $
    it "widens maxBound" $
      Word16.toInt64 maxBound `shouldBe` 65535

  describe "toInt (infallible)" $
    it "widens maxBound" $
      Word16.toInt maxBound `shouldBe` 65535

  describe "toInteger (infallible)" $
    it "widens maxBound" $
      Word16.toInteger maxBound `shouldBe` 65535

  describe "toFloat (infallible)" $
    it "converts maxBound" $
      Word16.toFloat maxBound `shouldBe` 65535.0

  describe "toDouble (infallible)" $
    it "converts maxBound" $
      Word16.toDouble maxBound `shouldBe` 65535.0
