{-# LANGUAGE ScopedTypeVariables #-}
module Test.Convert.PropertySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Data.Int
import Data.Word
import qualified Unwitch.Convert.Int8 as Int8
import qualified Unwitch.Convert.Int16 as Int16
import qualified Unwitch.Convert.Int32 as Int32
import qualified Unwitch.Convert.Int64 as Int64
import qualified Unwitch.Convert.Int as Int
import qualified Unwitch.Convert.Word8 as Word8
import qualified Unwitch.Convert.Word16 as Word16
import qualified Unwitch.Convert.Word32 as Word32
import qualified Unwitch.Convert.Word64 as Word64
import qualified Unwitch.Convert.Natural as Natural
import qualified Unwitch.Convert.Integer as Integer
import qualified Unwitch.Convert.Word as Word
import qualified Unwitch.Convert.Char as Char
import qualified Unwitch.Convert.Float as Float
import qualified Unwitch.Convert.Double as Double
import Numeric.Natural (Natural)

spec :: Spec
spec = describe "Property tests" $ do

  describe "Round-trip: widen then narrow recovers original" $ do
    prop "Int8 -> Int16 -> Int8" $ \(x :: Int8) ->
      Int16.toInt8 (Int8.toInt16 x) `shouldBe` Just x

    prop "Int8 -> Int32 -> Int8" $ \(x :: Int8) ->
      Int32.toInt8 (Int8.toInt32 x) `shouldBe` Just x

    prop "Int8 -> Int64 -> Int8" $ \(x :: Int8) ->
      Int64.toInt8 (Int8.toInt64 x) `shouldBe` Just x

    prop "Int8 -> Integer -> Int8" $ \(x :: Int8) ->
      Integer.toInt8 (Int8.toInteger x) `shouldBe` Just x

    prop "Int16 -> Int32 -> Int16" $ \(x :: Int16) ->
      Int32.toInt16 (Int16.toInt32 x) `shouldBe` Just x

    prop "Int16 -> Int64 -> Int16" $ \(x :: Int16) ->
      Int64.toInt16 (Int16.toInt64 x) `shouldBe` Just x

    prop "Int32 -> Int64 -> Int32" $ \(x :: Int32) ->
      Int64.toInt32 (Int32.toInt64 x) `shouldBe` Just x

    prop "Word8 -> Word16 -> Word8" $ \(x :: Word8) ->
      Word16.toWord8 (Word8.toWord16 x) `shouldBe` Just x

    prop "Word8 -> Word32 -> Word8" $ \(x :: Word8) ->
      Word32.toWord8 (Word8.toWord32 x) `shouldBe` Just x

    prop "Word8 -> Word64 -> Word8" $ \(x :: Word8) ->
      Word64.toWord8 (Word8.toWord64 x) `shouldBe` Just x

    prop "Word16 -> Word32 -> Word16" $ \(x :: Word16) ->
      Word32.toWord16 (Word16.toWord32 x) `shouldBe` Just x

    prop "Word16 -> Word64 -> Word16" $ \(x :: Word16) ->
      Word64.toWord16 (Word16.toWord64 x) `shouldBe` Just x

    prop "Word32 -> Word64 -> Word32" $ \(x :: Word32) ->
      Word64.toWord32 (Word32.toWord64 x) `shouldBe` Just x

    prop "Word8 -> Int16 -> Word8" $ \(x :: Word8) ->
      Int16.toWord8 (Word8.toInt16 x) `shouldBe` Just x

    prop "Word8 -> Int32 -> Word8" $ \(x :: Word8) ->
      Int32.toWord8 (Word8.toInt32 x) `shouldBe` Just x

    -- Int via Int
    prop "Int8 -> Int -> Int8" $ \(x :: Int8) ->
      Int.toInt8 (Int8.toInt x) `shouldBe` Just x

    prop "Int16 -> Int -> Int16" $ \(x :: Int16) ->
      Int.toInt16 (Int16.toInt x) `shouldBe` Just x

    -- Int via Integer
    prop "Int16 -> Integer -> Int16" $ \(x :: Int16) ->
      Integer.toInt16 (Int16.toInteger x) `shouldBe` Just x

    prop "Int32 -> Integer -> Int32" $ \(x :: Int32) ->
      Integer.toInt32 (Int32.toInteger x) `shouldBe` Just x

    prop "Int64 -> Integer -> Int64" $ \(x :: Int64) ->
      Integer.toInt64 (Int64.toInteger x) `shouldBe` Just x

    prop "Int -> Int64 -> Int" $ \(x :: Int) ->
      Int64.toInt (Int.toInt64 x) `shouldBe` Just x

    prop "Int -> Integer -> Int" $ \(x :: Int) ->
      Integer.toInt (Int.toInteger x) `shouldBe` Just x

    -- Word via Word
    prop "Word8 -> Word -> Word8" $ \(x :: Word8) ->
      Word.toWord8 (Word8.toWord x) `shouldBe` Just x

    prop "Word16 -> Word -> Word16" $ \(x :: Word16) ->
      Word.toWord16 (Word16.toWord x) `shouldBe` Just x

    -- Word via Natural
    prop "Word8 -> Natural -> Word8" $ \(x :: Word8) ->
      Natural.toWord8 (Word8.toNatural x) `shouldBe` Just x

    prop "Word16 -> Natural -> Word16" $ \(x :: Word16) ->
      Natural.toWord16 (Word16.toNatural x) `shouldBe` Just x

    prop "Word32 -> Natural -> Word32" $ \(x :: Word32) ->
      Natural.toWord32 (Word32.toNatural x) `shouldBe` Just x

    prop "Word64 -> Natural -> Word64" $ \(x :: Word64) ->
      Natural.toWord64 (Word64.toNatural x) `shouldBe` Just x

    prop "Word -> Natural -> Word" $ \(x :: Word) ->
      Natural.toWord (Word.toNatural x) `shouldBe` Just x

    -- Word via Integer
    prop "Word64 -> Integer -> Word64" $ \(x :: Word64) ->
      Integer.toWord64 (Word64.toInteger x) `shouldBe` Just x

    prop "Word -> Word64 -> Word" $ \(x :: Word) ->
      Word64.toWord (Word.toWord64 x) `shouldBe` Just x

    prop "Word -> Integer -> Word" $ \(x :: Word) ->
      Integer.toWord (Word.toInteger x) `shouldBe` Just x

    -- Cross-sign: Word via signed
    prop "Word8 -> Int64 -> Word8" $ \(x :: Word8) ->
      Int64.toWord8 (Word8.toInt64 x) `shouldBe` Just x

    prop "Word8 -> Int -> Word8" $ \(x :: Word8) ->
      Int.toWord8 (Word8.toInt x) `shouldBe` Just x

    prop "Word8 -> Integer -> Word8" $ \(x :: Word8) ->
      Integer.toWord8 (Word8.toInteger x) `shouldBe` Just x

    prop "Word16 -> Int32 -> Word16" $ \(x :: Word16) ->
      Int32.toWord16 (Word16.toInt32 x) `shouldBe` Just x

    prop "Word16 -> Int64 -> Word16" $ \(x :: Word16) ->
      Int64.toWord16 (Word16.toInt64 x) `shouldBe` Just x

    prop "Word16 -> Int -> Word16" $ \(x :: Word16) ->
      Int.toWord16 (Word16.toInt x) `shouldBe` Just x

    prop "Word16 -> Integer -> Word16" $ \(x :: Word16) ->
      Integer.toWord16 (Word16.toInteger x) `shouldBe` Just x

    prop "Word32 -> Int64 -> Word32" $ \(x :: Word32) ->
      Int64.toWord32 (Word32.toInt64 x) `shouldBe` Just x

    prop "Word32 -> Integer -> Word32" $ \(x :: Word32) ->
      Integer.toWord32 (Word32.toInteger x) `shouldBe` Just x

    -- Natural via Integer (Either pattern, generated via Word64)
    prop "Natural -> Integer -> Natural" $ \(w :: Word64) ->
      let x = fromIntegral w :: Natural
      in Integer.toNatural (Natural.toInteger x) `shouldBe` Right x

    -- Char
    prop "Char -> Int -> Char" $ \(x :: Char) ->
      Char.fromInt (Char.toInt x) `shouldBe` Just x

    prop "Char -> Word -> Char" $ \(x :: Char) ->
      Char.fromWord (Char.toWord x) `shouldBe` Just x

    -- Via Float (Either ViaIntegerErrors pattern)
    prop "Int8 -> Float -> Int8" $ \(x :: Int8) ->
      Float.toInt8 (Int8.toFloat x) `shouldBe` Right x

    prop "Int16 -> Float -> Int16" $ \(x :: Int16) ->
      Float.toInt16 (Int16.toFloat x) `shouldBe` Right x

    prop "Word8 -> Float -> Word8" $ \(x :: Word8) ->
      Float.toWord8 (Word8.toFloat x) `shouldBe` Right x

    prop "Word16 -> Float -> Word16" $ \(x :: Word16) ->
      Float.toWord16 (Word16.toFloat x) `shouldBe` Right x

    -- Via Double (Either ViaIntegerErrors pattern)
    prop "Int8 -> Double -> Int8" $ \(x :: Int8) ->
      Double.toInt8 (Int8.toDouble x) `shouldBe` Right x

    prop "Int16 -> Double -> Int16" $ \(x :: Int16) ->
      Double.toInt16 (Int16.toDouble x) `shouldBe` Right x

    prop "Int32 -> Double -> Int32" $ \(x :: Int32) ->
      Double.toInt32 (Int32.toDouble x) `shouldBe` Right x

    prop "Word8 -> Double -> Word8" $ \(x :: Word8) ->
      Double.toWord8 (Word8.toDouble x) `shouldBe` Right x

    prop "Word16 -> Double -> Word16" $ \(x :: Word16) ->
      Double.toWord16 (Word16.toDouble x) `shouldBe` Right x

    prop "Word32 -> Double -> Word32" $ \(x :: Word32) ->
      Double.toWord32 (Word32.toDouble x) `shouldBe` Right x

  describe "Fallible narrowing success agrees with fromIntegral" $ do
    prop "Int16 -> Int8: success implies fromIntegral match" $ \(x :: Int16) ->
      case Int16.toInt8 x of
        Just y  -> fromIntegral y `shouldBe` x
        Nothing -> pure ()

    prop "Int32 -> Int16: success implies fromIntegral match" $ \(x :: Int32) ->
      case Int32.toInt16 x of
        Just y  -> fromIntegral y `shouldBe` x
        Nothing -> pure ()

    prop "Int64 -> Int32: success implies fromIntegral match" $ \(x :: Int64) ->
      case Int64.toInt32 x of
        Just y  -> fromIntegral y `shouldBe` x
        Nothing -> pure ()

    prop "Word16 -> Word8: success implies fromIntegral match" $ \(x :: Word16) ->
      case Word16.toWord8 x of
        Just y  -> fromIntegral y `shouldBe` x
        Nothing -> pure ()

    prop "Word32 -> Word16: success implies fromIntegral match" $ \(x :: Word32) ->
      case Word32.toWord16 x of
        Just y  -> fromIntegral y `shouldBe` x
        Nothing -> pure ()

    prop "Word64 -> Word32: success implies fromIntegral match" $ \(x :: Word64) ->
      case Word64.toWord32 x of
        Just y  -> fromIntegral y `shouldBe` x
        Nothing -> pure ()

    prop "Integer -> Int8: success implies fromIntegral match" $ \(x :: Integer) ->
      case Integer.toInt8 x of
        Just y  -> fromIntegral y `shouldBe` x
        Nothing -> pure ()

    prop "Integer -> Word8: success implies fromIntegral match" $ \(x :: Integer) ->
      case Integer.toWord8 x of
        Just y  -> fromIntegral y `shouldBe` x
        Nothing -> pure ()

    prop "Int32 -> Int8: success implies fromIntegral match" $ \(x :: Int32) ->
      case Int32.toInt8 x of
        Just y  -> fromIntegral y `shouldBe` x
        Nothing -> pure ()

    prop "Int64 -> Int16: success implies fromIntegral match" $ \(x :: Int64) ->
      case Int64.toInt16 x of
        Just y  -> fromIntegral y `shouldBe` x
        Nothing -> pure ()

    prop "Int64 -> Int8: success implies fromIntegral match" $ \(x :: Int64) ->
      case Int64.toInt8 x of
        Just y  -> fromIntegral y `shouldBe` x
        Nothing -> pure ()

    prop "Word32 -> Word8: success implies fromIntegral match" $ \(x :: Word32) ->
      case Word32.toWord8 x of
        Just y  -> fromIntegral y `shouldBe` x
        Nothing -> pure ()

    prop "Word64 -> Word16: success implies fromIntegral match" $ \(x :: Word64) ->
      case Word64.toWord16 x of
        Just y  -> fromIntegral y `shouldBe` x
        Nothing -> pure ()

    prop "Word64 -> Word8: success implies fromIntegral match" $ \(x :: Word64) ->
      case Word64.toWord8 x of
        Just y  -> fromIntegral y `shouldBe` x
        Nothing -> pure ()

  describe "toNatural rejects iff negative" $ do
    prop "Int8 -> Natural: Left iff x < 0" $ \(x :: Int8) ->
      isLeft (Int8.toNatural x) `shouldBe` (x < 0)

    prop "Int16 -> Natural: Left iff x < 0" $ \(x :: Int16) ->
      isLeft (Int16.toNatural x) `shouldBe` (x < 0)

    prop "Int32 -> Natural: Left iff x < 0" $ \(x :: Int32) ->
      isLeft (Int32.toNatural x) `shouldBe` (x < 0)

    prop "Int64 -> Natural: Left iff x < 0" $ \(x :: Int64) ->
      isLeft (Int64.toNatural x) `shouldBe` (x < 0)

    prop "Int -> Natural: Left iff x < 0" $ \(x :: Int) ->
      isLeft (Int.toNatural x) `shouldBe` (x < 0)

    prop "Integer -> Natural: Left iff x < 0" $ \(x :: Integer) ->
      isLeft (Integer.toNatural x) `shouldBe` (x < 0)

  describe "toNatural success preserves value" $ do
    prop "Int64 -> Natural -> Integer round-trips via Integer" $ \(x :: Int64) ->
      case Int64.toNatural x of
        Right n  -> Natural.toInteger n `shouldBe` fromIntegral x
        Left _   -> pure ()

    prop "Word64 -> Natural -> Integer preserves value" $ \(x :: Word64) ->
      Natural.toInteger (Word64.toNatural x) `shouldBe` fromIntegral x

  describe "Float range check is exact at boundary" $ do
    prop "Int32 -> Float: succeeds iff abs value <= maxRepFloat" $ \(x :: Int32) ->
      let xi = fromIntegral x :: Integer
      in isRight (Int32.toFloat x) `shouldBe`
           (xi >= -maxRepFloat && xi <= maxRepFloat)

    prop "Word32 -> Float: succeeds iff value <= maxRepFloat" $ \(x :: Word32) ->
      isRight (Word32.toFloat x) `shouldBe`
        (fromIntegral x <= (maxRepFloat :: Integer))

    prop "Int64 -> Float: succeeds iff abs value <= maxRepFloat" $ \(x :: Int64) ->
      let xi = fromIntegral x :: Integer
      in isRight (Int64.toFloat x) `shouldBe`
           (xi >= -maxRepFloat && xi <= maxRepFloat)

    prop "Word64 -> Float: succeeds iff value <= maxRepFloat" $ \(x :: Word64) ->
      isRight (Word64.toFloat x) `shouldBe`
        (fromIntegral x <= (maxRepFloat :: Integer))

    prop "Integer -> Float: succeeds iff abs value <= maxRepFloat" $ \(x :: Integer) ->
      isRight (Integer.toFloat x) `shouldBe`
        (x >= -maxRepFloat && x <= maxRepFloat)

  describe "Double range check is exact at boundary" $ do
    prop "Int64 -> Double: succeeds iff abs value <= maxRepDouble" $ \(x :: Int64) ->
      let xi = fromIntegral x :: Integer
      in isRight (Int64.toDouble x) `shouldBe`
           (xi >= -maxRepDouble && xi <= maxRepDouble)

    prop "Word64 -> Double: succeeds iff value <= maxRepDouble" $ \(x :: Word64) ->
      isRight (Word64.toDouble x) `shouldBe`
        (fromIntegral x <= (maxRepDouble :: Integer))

    prop "Integer -> Double: succeeds iff abs value <= maxRepDouble" $ \(x :: Integer) ->
      isRight (Integer.toDouble x) `shouldBe`
        (x >= -maxRepDouble && x <= maxRepDouble)

  describe "Path independence: different widening paths agree" $ do
    prop "Int8: toInt32 == toInt16 >> Int16.toInt32" $ \(x :: Int8) ->
      Int8.toInt32 x `shouldBe` Int16.toInt32 (Int8.toInt16 x)

    prop "Int8: toInt64 == toInt32 >> Int32.toInt64" $ \(x :: Int8) ->
      Int8.toInt64 x `shouldBe` Int32.toInt64 (Int8.toInt32 x)

    prop "Int8: toInteger == toInt64 >> Int64.toInteger" $ \(x :: Int8) ->
      Int8.toInteger x `shouldBe` Int64.toInteger (Int8.toInt64 x)

    prop "Word8: toWord32 == toWord16 >> Word16.toWord32" $ \(x :: Word8) ->
      Word8.toWord32 x `shouldBe` Word16.toWord32 (Word8.toWord16 x)

    prop "Word8: toWord64 == toWord32 >> Word32.toWord64" $ \(x :: Word8) ->
      Word8.toWord64 x `shouldBe` Word32.toWord64 (Word8.toWord32 x)

    prop "Word8: toInteger == toWord64 >> Word64.toInteger" $ \(x :: Word8) ->
      Word8.toInteger x `shouldBe` Word64.toInteger (Word8.toWord64 x)

  describe "Infallible conversions preserve numeric value (via Integer)" $ do
    prop "Int8.toFloat preserves value" $ \(x :: Int8) ->
      Int8.toFloat x `shouldBe` fromIntegral x

    prop "Int8.toDouble preserves value" $ \(x :: Int8) ->
      Int8.toDouble x `shouldBe` fromIntegral x

    prop "Int16.toFloat preserves value" $ \(x :: Int16) ->
      Int16.toFloat x `shouldBe` fromIntegral x

    prop "Int16.toDouble preserves value" $ \(x :: Int16) ->
      Int16.toDouble x `shouldBe` fromIntegral x

    prop "Word8.toFloat preserves value" $ \(x :: Word8) ->
      Word8.toFloat x `shouldBe` fromIntegral x

    prop "Word8.toDouble preserves value" $ \(x :: Word8) ->
      Word8.toDouble x `shouldBe` fromIntegral x

    prop "Word16.toFloat preserves value" $ \(x :: Word16) ->
      Word16.toFloat x `shouldBe` fromIntegral x

    prop "Word16.toDouble preserves value" $ \(x :: Word16) ->
      Word16.toDouble x `shouldBe` fromIntegral x

    prop "Int32.toDouble preserves value" $ \(x :: Int32) ->
      Int32.toDouble x `shouldBe` fromIntegral x

    prop "Word32.toDouble preserves value" $ \(x :: Word32) ->
      Word32.toDouble x `shouldBe` fromIntegral x

  describe "Signed-to-unsigned: Nothing iff negative or too large" $ do
    prop "Int8 -> Word8: Nothing iff x < 0" $ \(x :: Int8) ->
      (Int8.toWord8 x == Nothing) `shouldBe` (x < 0)

    prop "Int16 -> Word16: Nothing iff x < 0" $ \(x :: Int16) ->
      (Int16.toWord16 x == Nothing) `shouldBe` (x < 0)

    prop "Int32 -> Word32: Nothing iff x < 0" $ \(x :: Int32) ->
      (Int32.toWord32 x == Nothing) `shouldBe` (x < 0)

    prop "Int64 -> Word64: Nothing iff x < 0" $ \(x :: Int64) ->
      (Int64.toWord64 x == Nothing) `shouldBe` (x < 0)

  describe "Cross-sign narrowing: exact failure condition" $ do
    prop "Int16 -> Word8: Nothing iff x < 0 || x > 255" $ \(x :: Int16) ->
      (Int16.toWord8 x == Nothing) `shouldBe` (x < 0 || x > 255)

    prop "Int32 -> Word16: Nothing iff x < 0 || x > 65535" $ \(x :: Int32) ->
      (Int32.toWord16 x == Nothing) `shouldBe` (x < 0 || x > 65535)

    prop "Int64 -> Word32: Nothing iff x < 0 || x > 4294967295" $ \(x :: Int64) ->
      (Int64.toWord32 x == Nothing) `shouldBe` (x < 0 || x > 4294967295)

    prop "Word16 -> Int8: Nothing iff x > 127" $ \(x :: Word16) ->
      (Word16.toInt8 x == Nothing) `shouldBe` (x > 127)

    prop "Word32 -> Int16: Nothing iff x > 32767" $ \(x :: Word32) ->
      (Word32.toInt16 x == Nothing) `shouldBe` (x > 32767)

    prop "Word64 -> Int32: Nothing iff x > 2147483647" $ \(x :: Word64) ->
      (Word64.toInt32 x == Nothing) `shouldBe` (x > 2147483647)

  describe "Float/Double to integer: exact success condition" $ do
    prop "Float -> Integer: succeeds iff finite and whole" $ \(x :: Float) ->
      isRight (Float.toInteger x) `shouldBe` isWholeFloat x

    prop "Float -> Int8: succeeds iff finite, whole, in [-128,127]" $ \(x :: Float) ->
      let i = truncate x :: Integer
      in isRight (Float.toInt8 x) `shouldBe`
           (isWholeFloat x && i >= -128 && i <= 127)

    prop "Float -> Word8: succeeds iff finite, whole, in [0,255]" $ \(x :: Float) ->
      let i = truncate x :: Integer
      in isRight (Float.toWord8 x) `shouldBe`
           (isWholeFloat x && i >= 0 && i <= 255)

    prop "Double -> Integer: succeeds iff finite and whole" $ \(x :: Double) ->
      isRight (Double.toInteger x) `shouldBe` isWholeDouble x

    prop "Double -> Int8: succeeds iff finite, whole, in [-128,127]" $ \(x :: Double) ->
      let i = truncate x :: Integer
      in isRight (Double.toInt8 x) `shouldBe`
           (isWholeDouble x && i >= -128 && i <= 127)

    prop "Double -> Word8: succeeds iff finite, whole, in [0,255]" $ \(x :: Double) ->
      let i = truncate x :: Integer
      in isRight (Double.toWord8 x) `shouldBe`
           (isWholeDouble x && i >= 0 && i <= 255)

  describe "Integer to Float/Double round-trip via toInteger" $ do
    prop "Int32 -> Float -> Integer: success round-trips" $ \(x :: Int32) ->
      case Int32.toFloat x of
        Right y  -> Float.toInteger y `shouldBe` Right (fromIntegral x)
        Left _   -> pure ()

    prop "Int64 -> Double -> Integer: success round-trips" $ \(x :: Int64) ->
      case Int64.toDouble x of
        Right y  -> Double.toInteger y `shouldBe` Right (fromIntegral x)
        Left _   -> pure ()

    prop "Word32 -> Float -> Integer: success round-trips" $ \(x :: Word32) ->
      case Word32.toFloat x of
        Right y  -> Float.toInteger y `shouldBe` Right (fromIntegral x)
        Left _   -> pure ()

    prop "Word64 -> Double -> Integer: success round-trips" $ \(x :: Word64) ->
      case Word64.toDouble x of
        Right y  -> Double.toInteger y `shouldBe` Right (fromIntegral x)
        Left _   -> pure ()

  describe "Float to Double round-trip" $ do
    prop "Float -> Double -> Float preserves value" $ \(x :: Float) ->
      let y = Double.toFloat (Float.toDouble x)
      in if isNaN x
         then y `shouldSatisfy` isNaN
         else y `shouldBe` x

maxRepFloat :: Num a => a
maxRepFloat = 16777215

maxRepDouble :: Num a => a
maxRepDouble = 9007199254740991

isLeft :: Either a b -> Bool
isLeft (Left _)  = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False

isWholeFloat :: Float -> Bool
isWholeFloat x = not (isNaN x) && not (isInfinite x)
  && x == fromIntegral (truncate x :: Integer)

isWholeDouble :: Double -> Bool
isWholeDouble x = not (isNaN x) && not (isInfinite x)
  && x == fromIntegral (truncate x :: Integer)
