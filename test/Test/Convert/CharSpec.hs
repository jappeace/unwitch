module Test.Convert.CharSpec (spec) where

import Test.Hspec
import qualified Unwitch.Convert.Char as Char

spec :: Spec
spec = describe "Unwitch.Convert.Char" $ do

  describe "toInt" $ do
    it "converts 'A' to 65" $
      Char.toInt 'A' `shouldBe` 65
    it "converts '0' to 48" $
      Char.toInt '0' `shouldBe` 48

  describe "toWord" $ do
    it "converts 'A' to 65" $
      Char.toWord 'A' `shouldBe` (65 :: Word)
    it "converts null to 0" $
      Char.toWord '\0' `shouldBe` (0 :: Word)

  describe "fromInt" $ do
    it "succeeds for valid codepoint" $
      Char.fromInt 65 `shouldBe` Just 'A'
    it "fails for negative" $
      Char.fromInt (-1) `shouldBe` Nothing
    it "fails for value > 0x10FFFF" $
      Char.fromInt 0x110000 `shouldBe` Nothing
    it "fails for surrogate" $
      Char.fromInt 0xD800 `shouldBe` Nothing

  describe "fromWord" $ do
    it "succeeds for valid codepoint" $
      Char.fromWord 65 `shouldBe` Just 'A'
    it "fails for value > 0x10FFFF" $
      Char.fromWord 0x110000 `shouldBe` Nothing
    it "fails for surrogate" $
      Char.fromWord 0xD800 `shouldBe` Nothing

  describe "round-trip" $
    it "fromInt . toInt == Just" $
      Char.fromInt (Char.toInt 'Z') `shouldBe` Just 'Z'
