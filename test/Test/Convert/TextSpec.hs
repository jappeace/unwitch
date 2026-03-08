module Test.Convert.TextSpec (spec) where

import Test.Hspec
import Data.Text qualified as T
import qualified Unwitch.Convert.Text as Text
import qualified Unwitch.Convert.LazyText as LazyText
import qualified Unwitch.Convert.ByteString as ByteString

spec :: Spec
spec = describe "Unwitch.Convert.Text" $ do

  describe "toLazyText / toText round-trip" $
    it "round-trips" $
      let t = T.pack "hello world"
      in LazyText.toText (Text.toLazyText t) `shouldBe` t

  describe "toString / fromString" $ do
    it "round-trips" $
      let s = "test string"
      in Text.toString (Text.fromString s) `shouldBe` s
    it "handles unicode" $
      let s = "\x00E9\x00FC\x00F1"
      in Text.toString (Text.fromString s) `shouldBe` s

  describe "toByteStringUtf8" $ do
    it "encodes ASCII" $
      Text.toByteStringUtf8 "hello" `shouldBe` "hello"
    it "round-trips with ByteString.toTextUtf8" $
      let t = T.pack "caf\x00E9"
      in ByteString.toTextUtf8 (Text.toByteStringUtf8 t) `shouldBe` Right t

  describe "toByteStringUtf16LE" $
    it "produces valid UTF-16LE output" $
      let t = T.pack "A"
          bs = Text.toByteStringUtf16LE t
      in ByteString.toTextUtf16LE bs `shouldBe` Right t

  describe "toByteStringUtf16BE" $
    it "produces valid UTF-16BE output" $
      let t = T.pack "B"
          bs = Text.toByteStringUtf16BE t
      in ByteString.toTextUtf16BE bs `shouldBe` Right t

  describe "toByteStringUtf32LE" $
    it "produces valid UTF-32LE output" $
      let t = T.pack "C"
          bs = Text.toByteStringUtf32LE t
      in ByteString.toTextUtf32LE bs `shouldBe` Right t

  describe "toByteStringUtf32BE" $
    it "produces valid UTF-32BE output" $
      let t = T.pack "D"
          bs = Text.toByteStringUtf32BE t
      in ByteString.toTextUtf32BE bs `shouldBe` Right t

  describe "toByteStringLatin1" $ do
    it "succeeds for ASCII text" $
      Text.toByteStringLatin1 "hello" `shouldSatisfy` \case
        Just _  -> True
        Nothing -> False
    it "succeeds for Latin1 range chars" $
      let t = T.pack "\x00E9\x00FC"
      in case Text.toByteStringLatin1 t of
           Just bs -> ByteString.toTextLatin1 bs `shouldBe` t
           Nothing -> expectationFailure "expected Just"
    it "fails for chars above 0xFF" $
      let t = T.pack "\x0100" -- Latin Extended-A
      in Text.toByteStringLatin1 t `shouldBe` Nothing
