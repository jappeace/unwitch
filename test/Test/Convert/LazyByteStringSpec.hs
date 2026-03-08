module Test.Convert.LazyByteStringSpec (spec) where

import Test.Hspec
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word8)
import qualified Unwitch.Convert.LazyByteString as LazyByteString
import qualified Unwitch.Convert.ByteString as ByteString

spec :: Spec
spec = describe "Unwitch.Convert.LazyByteString" $ do

  describe "toByteString / toLazyByteString round-trip" $
    it "round-trips" $
      let lbs = LBS.pack [10, 20, 30]
      in ByteString.toLazyByteString (LazyByteString.toByteString lbs) `shouldBe` lbs

  describe "toWord8s / fromWord8s" $ do
    it "round-trips" $
      let ws = [1, 2, 3, 4] :: [Word8]
      in LazyByteString.toWord8s (LazyByteString.fromWord8s ws) `shouldBe` ws
    it "empty list" $
      LazyByteString.toWord8s (LazyByteString.fromWord8s []) `shouldBe` ([] :: [Word8])

  describe "toLazyTextLatin1" $
    it "decodes latin1 bytes" $
      let lbs = LBS.pack [0xFC] -- u-umlaut
      in LazyByteString.toLazyTextLatin1 lbs `shouldBe` "\x00FC"

  describe "toLazyTextUtf8" $ do
    it "decodes valid UTF-8" $
      LazyByteString.toLazyTextUtf8 "hello" `shouldBe` Right "hello"
    it "fails on invalid UTF-8" $
      let lbs = LBS.pack [0xFF, 0xFE]
      in case LazyByteString.toLazyTextUtf8 lbs of
           Left _  -> pure ()
           Right _ -> expectationFailure "expected Left"

  describe "toLazyTextUtf16LE" $
    it "decodes valid UTF-16LE" $
      let lbs = LBS.pack [0x41, 0x00]
      in LazyByteString.toLazyTextUtf16LE lbs `shouldBe` Right "A"

  describe "toLazyTextUtf16BE" $
    it "decodes valid UTF-16BE" $
      let lbs = LBS.pack [0x00, 0x42]
      in LazyByteString.toLazyTextUtf16BE lbs `shouldBe` Right "B"

  describe "toLazyTextUtf32LE" $
    it "decodes valid UTF-32LE" $
      let lbs = LBS.pack [0x43, 0x00, 0x00, 0x00]
      in LazyByteString.toLazyTextUtf32LE lbs `shouldBe` Right "C"

  describe "toLazyTextUtf32BE" $
    it "decodes valid UTF-32BE" $
      let lbs = LBS.pack [0x00, 0x00, 0x00, 0x44]
      in LazyByteString.toLazyTextUtf32BE lbs `shouldBe` Right "D"
