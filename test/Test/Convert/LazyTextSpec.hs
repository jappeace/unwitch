module Test.Convert.LazyTextSpec (spec) where

import Test.Hspec
import Data.Text.Lazy qualified as LT
import qualified Unwitch.Convert.LazyText as LazyText
import qualified Unwitch.Convert.Text as Text
import qualified Unwitch.Convert.LazyByteString as LazyByteString

spec :: Spec
spec = describe "Unwitch.Convert.LazyText" $ do

  describe "toText / toLazyText round-trip" $
    it "round-trips" $
      let lt = LT.pack "hello world"
      in Text.toLazyText (LazyText.toText lt) `shouldBe` lt

  describe "toString / fromString" $ do
    it "round-trips" $
      let s = "test string"
      in LazyText.toString (LazyText.fromString s) `shouldBe` s
    it "handles unicode" $
      let s = "\x00E9\x00FC\x00F1"
      in LazyText.toString (LazyText.fromString s) `shouldBe` s

  describe "toLazyByteStringUtf8" $ do
    it "encodes ASCII" $
      LazyText.toLazyByteStringUtf8 "hello" `shouldBe` "hello"
    it "round-trips with LazyByteString.toLazyTextUtf8" $
      let lt = LT.pack "caf\x00E9"
      in LazyByteString.toLazyTextUtf8 (LazyText.toLazyByteStringUtf8 lt) `shouldBe` Right lt

  describe "toLazyByteStringUtf16LE" $
    it "produces valid output" $
      let lt = LT.pack "A"
      in LazyByteString.toLazyTextUtf16LE (LazyText.toLazyByteStringUtf16LE lt) `shouldBe` Right lt

  describe "toLazyByteStringUtf16BE" $
    it "produces valid output" $
      let lt = LT.pack "B"
      in LazyByteString.toLazyTextUtf16BE (LazyText.toLazyByteStringUtf16BE lt) `shouldBe` Right lt

  describe "toLazyByteStringUtf32LE" $
    it "produces valid output" $
      let lt = LT.pack "C"
      in LazyByteString.toLazyTextUtf32LE (LazyText.toLazyByteStringUtf32LE lt) `shouldBe` Right lt

  describe "toLazyByteStringUtf32BE" $
    it "produces valid output" $
      let lt = LT.pack "D"
      in LazyByteString.toLazyTextUtf32BE (LazyText.toLazyByteStringUtf32BE lt) `shouldBe` Right lt

  describe "toLazyByteStringLatin1" $ do
    it "succeeds for Latin1 range chars" $
      let lt = LT.pack "\x00E9\x00FC"
      in case LazyText.toLazyByteStringLatin1 lt of
           Just lbs -> LazyByteString.toLazyTextLatin1 lbs `shouldBe` lt
           Nothing  -> expectationFailure "expected Just"
    it "fails for chars above 0xFF" $
      let lt = LT.pack "\x0100"
      in LazyText.toLazyByteStringLatin1 lt `shouldBe` Nothing
