module Test.Convert.ByteStringSpec (spec) where

import Test.Hspec
import Data.ByteString qualified as BS
import Data.Word (Word8)
import qualified Unwitch.Convert.ByteString as ByteString
import qualified Unwitch.Convert.LazyByteString as LazyByteString
import qualified Unwitch.Convert.ShortByteString as ShortByteString

spec :: Spec
spec = describe "Unwitch.Convert.ByteString" $ do

  describe "toLazyByteString / toByteString round-trip" $
    it "round-trips" $
      let bs = BS.pack [104, 101, 108, 108, 111]
      in LazyByteString.toByteString (ByteString.toLazyByteString bs) `shouldBe` bs

  describe "toShortByteString round-trip" $
    it "round-trips with ShortByteString.toByteString" $
      let bs = BS.pack [1, 2, 3]
      in ShortByteString.toByteString (ByteString.toShortByteString bs) `shouldBe` bs

  describe "toWord8s / fromWord8s" $ do
    it "round-trips" $
      let ws = [72, 105] :: [Word8]
      in ByteString.toWord8s (ByteString.fromWord8s ws) `shouldBe` ws
    it "empty list" $
      ByteString.toWord8s (ByteString.fromWord8s []) `shouldBe` ([] :: [Word8])

  describe "toTextLatin1" $
    it "decodes latin1 bytes" $
      let bs = BS.pack [0xE9, 0xE8] -- e-acute, e-grave
      in ByteString.toTextLatin1 bs `shouldBe` "\x00E9\x00E8"

  describe "toTextUtf8" $ do
    it "decodes valid UTF-8" $
      ByteString.toTextUtf8 "hello" `shouldBe` Right "hello"
    it "fails on invalid UTF-8" $
      let bs = BS.pack [0xFF, 0xFE]
      in case ByteString.toTextUtf8 bs of
           Left _  -> pure ()
           Right _ -> expectationFailure "expected Left"

  describe "toTextUtf16LE" $ do
    it "decodes valid UTF-16LE" $
      let bs = BS.pack [0x41, 0x00] -- 'A' in UTF-16LE
      in ByteString.toTextUtf16LE bs `shouldBe` Right "A"
    it "fails on truncated input" $
      let bs = BS.pack [0x41] -- odd byte count
      in case ByteString.toTextUtf16LE bs of
           Left _  -> pure ()
           Right _ -> expectationFailure "expected Left"

  describe "toTextUtf16BE" $
    it "decodes valid UTF-16BE" $
      let bs = BS.pack [0x00, 0x42] -- 'B' in UTF-16BE
      in ByteString.toTextUtf16BE bs `shouldBe` Right "B"

  describe "toTextUtf32LE" $
    it "decodes valid UTF-32LE" $
      let bs = BS.pack [0x43, 0x00, 0x00, 0x00] -- 'C' in UTF-32LE
      in ByteString.toTextUtf32LE bs `shouldBe` Right "C"

  describe "toTextUtf32BE" $ do
    it "decodes valid UTF-32BE" $
      let bs = BS.pack [0x00, 0x00, 0x00, 0x44] -- 'D' in UTF-32BE
      in ByteString.toTextUtf32BE bs `shouldBe` Right "D"
    it "fails on invalid input" $
      let bs = BS.pack [0x01] -- not 4-byte aligned
      in case ByteString.toTextUtf32BE bs of
           Left _  -> pure ()
           Right _ -> expectationFailure "expected Left"
