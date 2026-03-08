module Unwitch.Convert.Integer
  ( toDouble
  , toDouble#
  , toFloat
  , toFloat#
  , toNatural
  , toNatural#
  , toInt8
  , toInt8#
  , toInt16
  , toInt16#
  , toInt32
  , toInt32#
  , toInt64
  , toInt64#
  , toInt
  , toInt#
  , toWord8
  , toWord8#
  , toWord16
  , toWord16#
  , toWord32
  , toWord32#
  , toWord64
  , toWord64#
  , toWord
  , toWord#
  )
where

import           Unwitch.Errors
import           Unwitch.Constant
import qualified Data.Bits as Bits
import Data.Word
import Data.Int
import Numeric.Natural (Natural)


toDouble :: Integer -> Either Overflows Double
toDouble integer = if
    | integer < -maxIntegralRepDouble -> Left Underflow
    | integer > maxIntegralRepDouble -> Left Overflow
    | otherwise -> Right $ Prelude.fromIntegral integer

toFloat :: Integer -> Either Overflows Float
toFloat integer = if
    | integer < -maxIntegralRepFloat -> Left Underflow
    | integer > maxIntegralRepFloat -> Left Overflow
    | otherwise -> Right $ Prelude.fromIntegral integer

toNatural :: Integer -> Either Overflows Natural
toNatural integer = if
    | integer < 0 -> Left Underflow
    | otherwise -> Right $ Prelude.fromIntegral integer

toInt8 :: Integer -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Integer -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt32 :: Integer -> Maybe Int32
toInt32 = Bits.toIntegralSized

toInt64 :: Integer -> Maybe Int64
toInt64 = Bits.toIntegralSized

toInt :: Integer -> Maybe Int
toInt = Bits.toIntegralSized

toWord8 :: Integer -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Integer -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Integer -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord64 :: Integer -> Maybe Word64
toWord64 = Bits.toIntegralSized

toWord :: Integer -> Maybe Word
toWord = Bits.toIntegralSized

toDouble# :: Integer -> (# Overflows | Double #)
toDouble# x = case toDouble x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toFloat# :: Integer -> (# Overflows | Float #)
toFloat# x = case toFloat x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toNatural# :: Integer -> (# Overflows | Natural #)
toNatural# x = case toNatural x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toInt8# :: Integer -> (# Int8 | (# #) #)
toInt8# x = case toInt8 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toInt16# :: Integer -> (# Int16 | (# #) #)
toInt16# x = case toInt16 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toInt32# :: Integer -> (# Int32 | (# #) #)
toInt32# x = case toInt32 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toInt64# :: Integer -> (# Int64 | (# #) #)
toInt64# x = case toInt64 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toInt# :: Integer -> (# Int | (# #) #)
toInt# x = case toInt x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord8# :: Integer -> (# Word8 | (# #) #)
toWord8# x = case toWord8 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord16# :: Integer -> (# Word16 | (# #) #)
toWord16# x = case toWord16 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord32# :: Integer -> (# Word32 | (# #) #)
toWord32# x = case toWord32 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord64# :: Integer -> (# Word64 | (# #) #)
toWord64# x = case toWord64 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord# :: Integer -> (# Word | (# #) #)
toWord# x = case toWord x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)
