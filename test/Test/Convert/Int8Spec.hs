module Test.Convert.Int8Spec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Numeric.Natural (Natural)
import Unwitch.Errors
import qualified Unwitch.Convert.Int8 as Int8

spec :: Spec
spec = describe "Unwitch.Convert.Int8" $ do

  describe "toInt16 (infallible)" $ do
    it "converts minBound" $
      Int8.toInt16 minBound `shouldBe` (-128)
    it "converts maxBound" $
      Int8.toInt16 maxBound `shouldBe` 127
    it "converts 0" $
      Int8.toInt16 0 `shouldBe` 0

  describe "toInt32 (infallible)" $ do
    it "converts minBound" $
      Int8.toInt32 minBound `shouldBe` (-128)
    it "converts maxBound" $
      Int8.toInt32 maxBound `shouldBe` 127

  describe "toInt64 (infallible)" $ do
    it "converts minBound" $
      Int8.toInt64 minBound `shouldBe` (-128)
    it "converts maxBound" $
      Int8.toInt64 maxBound `shouldBe` 127

  describe "toInt (infallible)" $ do
    it "converts minBound" $
      Int8.toInt minBound `shouldBe` (-128)
    it "converts maxBound" $
      Int8.toInt maxBound `shouldBe` 127

  describe "toInteger (infallible)" $ do
    it "converts minBound" $
      Int8.toInteger minBound `shouldBe` (-128)
    it "converts maxBound" $
      Int8.toInteger maxBound `shouldBe` 127

  describe "toWord8 (fallible)" $ do
    it "rejects negative values" $
      Int8.toWord8 (-1 :: Int8) `shouldBe` Nothing
    it "converts 0" $
      Int8.toWord8 0 `shouldBe` Just 0
    it "converts maxBound" $
      Int8.toWord8 (127 :: Int8) `shouldBe` Just (127 :: Word8)

  describe "toWord16 (fallible)" $ do
    it "rejects negative values" $
      Int8.toWord16 (-1 :: Int8) `shouldBe` Nothing
    it "converts maxBound" $
      Int8.toWord16 (127 :: Int8) `shouldBe` Just (127 :: Word16)

  describe "toWord32 (fallible)" $ do
    it "rejects negative values" $
      Int8.toWord32 (-1 :: Int8) `shouldBe` Nothing
    it "converts maxBound" $
      Int8.toWord32 (127 :: Int8) `shouldBe` Just (127 :: Word32)

  describe "toWord64 (fallible)" $ do
    it "rejects negative values" $
      Int8.toWord64 (-1 :: Int8) `shouldBe` Nothing
    it "converts maxBound" $
      Int8.toWord64 (127 :: Int8) `shouldBe` Just (127 :: Word64)

  describe "toWord (fallible)" $ do
    it "rejects negative values" $
      Int8.toWord (-1 :: Int8) `shouldBe` Nothing
    it "converts maxBound" $
      Int8.toWord (127 :: Int8) `shouldBe` Just (127 :: Word)

  describe "toNatural" $ do
    it "rejects negative values" $
      Int8.toNatural (-1 :: Int8) `shouldBe` Left Underflow
    it "converts 0" $
      Int8.toNatural 0 `shouldBe` Right (0 :: Natural)
    it "converts maxBound" $
      Int8.toNatural (127 :: Int8) `shouldBe` Right (127 :: Natural)

  describe "toFloat (infallible)" $ do
    it "converts minBound" $
      Int8.toFloat minBound `shouldBe` (-128.0)
    it "converts maxBound" $
      Int8.toFloat maxBound `shouldBe` 127.0

  describe "toDouble (infallible)" $ do
    it "converts minBound" $
      Int8.toDouble minBound `shouldBe` (-128.0)
    it "converts maxBound" $
      Int8.toDouble maxBound `shouldBe` 127.0
