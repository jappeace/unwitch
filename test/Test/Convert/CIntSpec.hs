module Test.Convert.CIntSpec (spec) where

import Test.Hspec
import Data.Int
import Data.Word
import Foreign.C.Types (CInt)
import qualified Unwitch.Convert.CInt as CInt

spec :: Spec
spec = describe "Unwitch.Convert.CInt" $ do

  describe "toInt8 (fallible)" $ do
    it "narrows in-range" $
      CInt.toInt8 (100 :: CInt) `shouldBe` Just (100 :: Int8)
    it "rejects out-of-range" $
      CInt.toInt8 (200 :: CInt) `shouldBe` Nothing

  describe "toInt16 (fallible)" $ do
    it "converts in-range" $
      CInt.toInt16 (1000 :: CInt) `shouldBe` Just (1000 :: Int16)
    it "rejects out-of-range" $
      CInt.toInt16 (40000 :: CInt) `shouldBe` Nothing

  describe "toInt32 (total)" $
    it "unwraps CInt" $
      CInt.toInt32 (42 :: CInt) `shouldBe` (42 :: Int32)

  describe "toInt (total)" $ do
    it "converts positive" $
      CInt.toInt (42 :: CInt) `shouldBe` (42 :: Int)
    it "converts negative" $
      CInt.toInt (-1 :: CInt) `shouldBe` (-1 :: Int)

  describe "toWord8 (fallible)" $ do
    it "rejects negative" $
      CInt.toWord8 (-1 :: CInt) `shouldBe` Nothing
    it "converts in-range" $
      CInt.toWord8 (255 :: CInt) `shouldBe` Just (255 :: Word8)
    it "rejects too large" $
      CInt.toWord8 (256 :: CInt) `shouldBe` Nothing

  describe "toDouble (total)" $ do
    it "converts positive" $
      CInt.toDouble (42 :: CInt) `shouldBe` (42.0 :: Double)
    it "converts negative" $
      CInt.toDouble (-100 :: CInt) `shouldBe` (-100.0 :: Double)
