{-# LANGUAGE ScopedTypeVariables #-}
module Test.Convert.UnboxedSpec
  ( spec
  )
where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import qualified Unwitch.Convert.Integer as Integer
import qualified Unwitch.Convert.Double as Double
import qualified Unwitch.Convert.Float as Float
import qualified Unwitch.Convert.Ratio as Ratio
import qualified Unwitch.Convert.Int as Int
import qualified Unwitch.Convert.Int8 as Int8
import qualified Unwitch.Convert.Int16 as Int16
import qualified Unwitch.Convert.Int32 as Int32
import qualified Unwitch.Convert.Int64 as Int64
import qualified Unwitch.Convert.Word as Word
import qualified Unwitch.Convert.Word8 as Word8
import qualified Unwitch.Convert.Word16 as Word16
import qualified Unwitch.Convert.Word32 as Word32
import qualified Unwitch.Convert.Word64 as Word64
import qualified Unwitch.Convert.Natural as Natural
import qualified Unwitch.Convert.Char as Char
import qualified Unwitch.Convert.Complex as Complex
import qualified Unwitch.Convert.Fixed as Fixed
import qualified Unwitch.Convert.Text as Text
import qualified Unwitch.Convert.LazyText as LazyText
import qualified Unwitch.Convert.ByteString as ByteString
import qualified Unwitch.Convert.LazyByteString as LazyByteString

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Complex (Complex((:+)))
import Data.Fixed (Pico)
import Data.Int
import Data.Ratio ((%))
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Word
import Numeric.Natural (Natural)

spec :: Spec
spec = describe "Unboxed sum variants" $ do

  describe "Integer" $ do
    prop "toInt8# agrees with toInt8" $ \(x :: Integer) ->
      case Integer.toInt8# x of
        (# y | #)      -> Integer.toInt8 x `shouldBe` Just y
        (# | (# #) #)  -> Integer.toInt8 x `shouldBe` Nothing

    prop "toDouble# agrees with toDouble" $ \(x :: Integer) ->
      case Integer.toDouble# x of
        (# e | #) -> Integer.toDouble x `shouldBe` Left e
        (# | y #) -> Integer.toDouble x `shouldBe` Right y

  describe "Double" $ do
    prop "toInt8# agrees with toInt8" $ \(x :: Double) ->
      case Double.toInt8# x of
        (# e | #) -> Double.toInt8 x `shouldBe` Left e
        (# | y #) -> Double.toInt8 x `shouldBe` Right y

    prop "toRational# agrees with toRational" $ \(x :: Double) ->
      case Double.toRational# x of
        (# e | #) -> Double.toRational x `shouldBe` Left e
        (# | y #) -> Double.toRational x `shouldBe` Right y

    prop "toInteger# agrees with toInteger" $ \(x :: Double) ->
      case Double.toInteger# x of
        (# e | #) -> Double.toInteger x `shouldBe` Left e
        (# | y #) -> Double.toInteger x `shouldBe` Right y

  describe "Float" $ do
    prop "toInt8# agrees with toInt8" $ \(x :: Float) ->
      case Float.toInt8# x of
        (# e | #) -> Float.toInt8 x `shouldBe` Left e
        (# | y #) -> Float.toInt8 x `shouldBe` Right y

  describe "Ratio" $ do
    prop "unwrapIfDenominatorOne# agrees with unwrapIfDenominatorOne" $ \(n :: Integer) (d :: Integer) ->
      let r = if d == 0 then n % 1 else n % d
      in case Ratio.unwrapIfDenominatorOne# r of
        (# y | #)      -> Ratio.unwrapIfDenominatorOne r `shouldBe` Just y
        (# | (# #) #)  -> Ratio.unwrapIfDenominatorOne r `shouldBe` Nothing

  describe "Int" $ do
    prop "toInt8# agrees with toInt8" $ \(x :: Int) ->
      case Int.toInt8# x of
        (# y | #)      -> Int.toInt8 x `shouldBe` Just y
        (# | (# #) #)  -> Int.toInt8 x `shouldBe` Nothing

    prop "toNatural# agrees with toNatural" $ \(x :: Int) ->
      case Int.toNatural# x of
        (# e | #) -> Int.toNatural x `shouldBe` Left e
        (# | y #) -> Int.toNatural x `shouldBe` Right y

  describe "Int8" $ do
    prop "toWord8# agrees with toWord8" $ \(x :: Int8) ->
      case Int8.toWord8# x of
        (# y | #)      -> Int8.toWord8 x `shouldBe` Just y
        (# | (# #) #)  -> Int8.toWord8 x `shouldBe` Nothing

    prop "toNatural# agrees with toNatural" $ \(x :: Int8) ->
      case Int8.toNatural# x of
        (# e | #) -> Int8.toNatural x `shouldBe` Left e
        (# | y #) -> Int8.toNatural x `shouldBe` Right y

  describe "Int16" $ do
    prop "toInt8# agrees with toInt8" $ \(x :: Int16) ->
      case Int16.toInt8# x of
        (# y | #)      -> Int16.toInt8 x `shouldBe` Just y
        (# | (# #) #)  -> Int16.toInt8 x `shouldBe` Nothing

  describe "Int32" $ do
    prop "toInt8# agrees with toInt8" $ \(x :: Int32) ->
      case Int32.toInt8# x of
        (# y | #)      -> Int32.toInt8 x `shouldBe` Just y
        (# | (# #) #)  -> Int32.toInt8 x `shouldBe` Nothing

    prop "toFloat# agrees with toFloat" $ \(x :: Int32) ->
      case Int32.toFloat# x of
        (# e | #) -> Int32.toFloat x `shouldBe` Left e
        (# | y #) -> Int32.toFloat x `shouldBe` Right y

  describe "Int64" $ do
    prop "toInt8# agrees with toInt8" $ \(x :: Int64) ->
      case Int64.toInt8# x of
        (# y | #)      -> Int64.toInt8 x `shouldBe` Just y
        (# | (# #) #)  -> Int64.toInt8 x `shouldBe` Nothing

    prop "toDouble# agrees with toDouble" $ \(x :: Int64) ->
      case Int64.toDouble# x of
        (# e | #) -> Int64.toDouble x `shouldBe` Left e
        (# | y #) -> Int64.toDouble x `shouldBe` Right y

  describe "Word" $ do
    prop "toWord8# agrees with toWord8" $ \(x :: Word) ->
      case Word.toWord8# x of
        (# y | #)      -> Word.toWord8 x `shouldBe` Just y
        (# | (# #) #)  -> Word.toWord8 x `shouldBe` Nothing

    prop "toFloat# agrees with toFloat" $ \(x :: Word) ->
      case Word.toFloat# x of
        (# e | #) -> Word.toFloat x `shouldBe` Left e
        (# | y #) -> Word.toFloat x `shouldBe` Right y

  describe "Word8" $ do
    prop "toInt8# agrees with toInt8" $ \(x :: Word8) ->
      case Word8.toInt8# x of
        (# y | #)      -> Word8.toInt8 x `shouldBe` Just y
        (# | (# #) #)  -> Word8.toInt8 x `shouldBe` Nothing

  describe "Word16" $ do
    prop "toWord8# agrees with toWord8" $ \(x :: Word16) ->
      case Word16.toWord8# x of
        (# y | #)      -> Word16.toWord8 x `shouldBe` Just y
        (# | (# #) #)  -> Word16.toWord8 x `shouldBe` Nothing

  describe "Word32" $ do
    prop "toWord8# agrees with toWord8" $ \(x :: Word32) ->
      case Word32.toWord8# x of
        (# y | #)      -> Word32.toWord8 x `shouldBe` Just y
        (# | (# #) #)  -> Word32.toWord8 x `shouldBe` Nothing

    prop "toFloat# agrees with toFloat" $ \(x :: Word32) ->
      case Word32.toFloat# x of
        (# e | #) -> Word32.toFloat x `shouldBe` Left e
        (# | y #) -> Word32.toFloat x `shouldBe` Right y

  describe "Word64" $ do
    prop "toWord8# agrees with toWord8" $ \(x :: Word64) ->
      case Word64.toWord8# x of
        (# y | #)      -> Word64.toWord8 x `shouldBe` Just y
        (# | (# #) #)  -> Word64.toWord8 x `shouldBe` Nothing

    prop "toFloat# agrees with toFloat" $ \(x :: Word64) ->
      case Word64.toFloat# x of
        (# e | #) -> Word64.toFloat x `shouldBe` Left e
        (# | y #) -> Word64.toFloat x `shouldBe` Right y

  describe "Natural" $ do
    prop "toWord8# agrees with toWord8" $ \(w :: Word) ->
      let x = fromIntegral w :: Natural
      in case Natural.toWord8# x of
        (# y | #)      -> Natural.toWord8 x `shouldBe` Just y
        (# | (# #) #)  -> Natural.toWord8 x `shouldBe` Nothing

    prop "toFloat# agrees with toFloat" $ \(w :: Word) ->
      let x = fromIntegral w :: Natural
      in case Natural.toFloat# x of
        (# e | #) -> Natural.toFloat x `shouldBe` Left e
        (# | y #) -> Natural.toFloat x `shouldBe` Right y

  describe "Char" $ do
    prop "fromInt# agrees with fromInt" $ \(x :: Int) ->
      case Char.fromInt# x of
        (# y | #)      -> Char.fromInt x `shouldBe` Just y
        (# | (# #) #)  -> Char.fromInt x `shouldBe` Nothing

  describe "Complex" $ do
    prop "toReal# agrees with toReal" $ \(r :: Double) (i :: Double) ->
      let c = r :+ i
      in case Complex.toReal# c of
        (# y | #)      -> Complex.toReal c `shouldBe` Just y
        (# | (# #) #)  -> Complex.toReal c `shouldBe` Nothing

  describe "Fixed" $ do
    prop "toInteger# agrees with toInteger" $ \(x :: Integer) ->
      let fixed = fromInteger x :: Pico
      in case Fixed.toInteger# fixed of
        (# y | #)      -> Fixed.toInteger fixed `shouldBe` Just y
        (# | (# #) #)  -> Fixed.toInteger fixed `shouldBe` Nothing

  describe "Text" $ do
    prop "toByteStringLatin1# agrees with toByteStringLatin1" $ \(s :: String) ->
      let t = T.pack s
      in case Text.toByteStringLatin1# t of
        (# y | #)      -> Text.toByteStringLatin1 t `shouldBe` Just y
        (# | (# #) #)  -> Text.toByteStringLatin1 t `shouldBe` Nothing

  describe "LazyText" $ do
    prop "toLazyByteStringLatin1# agrees with toLazyByteStringLatin1" $ \(s :: String) ->
      let t = LT.pack s
      in case LazyText.toLazyByteStringLatin1# t of
        (# y | #)      -> LazyText.toLazyByteStringLatin1 t `shouldBe` Just y
        (# | (# #) #)  -> LazyText.toLazyByteStringLatin1 t `shouldBe` Nothing

  describe "ByteString" $ do
    prop "toTextUtf8# agrees with toTextUtf8" $ \(ws :: [Word8]) ->
      let bs = BS.pack ws
      in case ByteString.toTextUtf8# bs of
        (# e | #) -> ByteString.toTextUtf8 bs `shouldBe` Left e
        (# | y #) -> ByteString.toTextUtf8 bs `shouldBe` Right y

  describe "LazyByteString" $ do
    prop "toLazyTextUtf8# agrees with toLazyTextUtf8" $ \(ws :: [Word8]) ->
      let bs = LBS.pack ws
      in case LazyByteString.toLazyTextUtf8# bs of
        (# e | #) -> LazyByteString.toLazyTextUtf8 bs `shouldBe` Left e
        (# | y #) -> LazyByteString.toLazyTextUtf8 bs `shouldBe` Right y
