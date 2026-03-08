module Unwitch.Convert.Word64
  ( toWord8
  , toWord16
  , toWord32
  , toWord
  , toNatural
  , toInt8
  , toInt16
  , toInt32
  , toInt64
  , toInt
  , toInteger
  , toFloat
  , toDouble
  , toWord8#
  , toWord16#
  , toWord32#
  , toWord#
  , toInt8#
  , toInt16#
  , toInt32#
  , toInt64#
  , toInt#
  , toFloat#
  , toDouble#
  )
where

import           Unwitch.Errors
import           Unwitch.Constant
import qualified Data.Bits as Bits
import           Data.Word
import           Data.Int
import           Numeric.Natural (Natural)
import           Prelude hiding (toInteger)

toWord8 :: Word64 -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Word64 -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Word64 -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord :: Word64 -> Maybe Word
toWord = Bits.toIntegralSized

toNatural :: Word64 -> Natural
toNatural = fromIntegral

toInt8 :: Word64 -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Word64 -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt32 :: Word64 -> Maybe Int32
toInt32 = Bits.toIntegralSized

toInt64 :: Word64 -> Maybe Int64
toInt64 = Bits.toIntegralSized

toInt :: Word64 -> Maybe Int
toInt = Bits.toIntegralSized

toInteger :: Word64 -> Integer
toInteger = fromIntegral

toFloat :: Word64 -> Either Overflows Float
toFloat x = if
  | x > maxIntegralRepFloat -> Left Overflow
  | otherwise               -> Right $ fromIntegral x

toDouble :: Word64 -> Either Overflows Double
toDouble x = if
  | x > maxIntegralRepDouble -> Left Overflow
  | otherwise                -> Right $ fromIntegral x

toWord8# :: Word64 -> (# Word8 | (# #) #)
toWord8# x = case toWord8 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord16# :: Word64 -> (# Word16 | (# #) #)
toWord16# x = case toWord16 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord32# :: Word64 -> (# Word32 | (# #) #)
toWord32# x = case toWord32 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord# :: Word64 -> (# Word | (# #) #)
toWord# x = case toWord x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toInt8# :: Word64 -> (# Int8 | (# #) #)
toInt8# x = case toInt8 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toInt16# :: Word64 -> (# Int16 | (# #) #)
toInt16# x = case toInt16 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toInt32# :: Word64 -> (# Int32 | (# #) #)
toInt32# x = case toInt32 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toInt64# :: Word64 -> (# Int64 | (# #) #)
toInt64# x = case toInt64 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toInt# :: Word64 -> (# Int | (# #) #)
toInt# x = case toInt x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toFloat# :: Word64 -> (# Overflows | Float #)
toFloat# x = case toFloat x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toDouble# :: Word64 -> (# Overflows | Double #)
toDouble# x = case toDouble x of
  Left e  -> (# e | #)
  Right y -> (# | y #)
