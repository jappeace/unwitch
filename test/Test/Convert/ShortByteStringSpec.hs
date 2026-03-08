module Test.Convert.ShortByteStringSpec (spec) where

import Test.Hspec
import Data.ByteString.Short qualified as SBS
import Data.Word (Word8)
import qualified Unwitch.Convert.ShortByteString as ShortByteString
import qualified Unwitch.Convert.ByteString as ByteString

spec :: Spec
spec = describe "Unwitch.Convert.ShortByteString" $ do

  describe "toByteString round-trip" $
    it "round-trips with ByteString.toShortByteString" $
      let sbs = SBS.pack [5, 10, 15]
      in ByteString.toShortByteString (ShortByteString.toByteString sbs) `shouldBe` sbs

  describe "toWord8s / fromWord8s" $ do
    it "round-trips" $
      let ws = [100, 200, 255] :: [Word8]
      in ShortByteString.toWord8s (ShortByteString.fromWord8s ws) `shouldBe` ws
    it "empty list" $
      ShortByteString.toWord8s (ShortByteString.fromWord8s []) `shouldBe` ([] :: [Word8])
