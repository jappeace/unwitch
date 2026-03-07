module Test.Convert.WordSpec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Unwitch.Errors
import qualified Unwitch.Convert.Word as Word

spec :: Spec
spec = describe "Unwitch.Convert.Word" $ do

  describe "toWord8 (fallible)" $
    it "rejects too large" $
      Word.toWord8 (256 :: Word) `shouldBe` Nothing

  describe "toWord16 (fallible)" $
    it "rejects too large" $
      Word.toWord16 (65536 :: Word) `shouldBe` Nothing

  describe "toWord32 (fallible)" $
    it "converts in-range" $
      Word.toWord32 (100 :: Word) `shouldBe` Just (100 :: Word32)

  describe "toWord64 (infallible)" $
    it "widens maxBound" $
      Word.toWord64 maxBound `shouldBe` fromIntegral (maxBound :: Word)

  describe "toNatural (infallible)" $
    it "converts maxBound" $
      Word.toNatural maxBound `shouldBe` fromIntegral (maxBound :: Word)

  describe "toInt8 (fallible)" $
    it "rejects too large" $
      Word.toInt8 (200 :: Word) `shouldBe` Nothing

  describe "toInt16 (fallible)" $
    it "rejects too large" $
      Word.toInt16 (40000 :: Word) `shouldBe` Nothing

  describe "toInt32 (fallible)" $
    it "rejects too large" $
      Word.toInt32 (3000000000 :: Word) `shouldBe` Nothing

  describe "toInt64 (fallible)" $
    it "converts in-range" $
      Word.toInt64 (100 :: Word) `shouldBe` Just (100 :: Int64)

  describe "toInt (fallible)" $ do
    it "rejects maxBound" $
      Word.toInt (maxBound :: Word) `shouldBe` Nothing
    it "converts 0" $
      Word.toInt 0 `shouldBe` Just (0 :: Int)

  describe "toInteger (infallible)" $
    it "converts maxBound" $
      Word.toInteger maxBound `shouldBe` fromIntegral (maxBound :: Word)

  describe "toFloat (range-checked)" $ do
    it "converts in-range" $
      Word.toFloat (16777215 :: Word) `shouldBe` Right 16777215.0
    it "rejects too large" $
      Word.toFloat (16777216 :: Word) `shouldBe` Left Overflow

  describe "toDouble (range-checked)" $
    it "converts 0" $
      Word.toDouble 0 `shouldBe` Right (0.0 :: Double)
