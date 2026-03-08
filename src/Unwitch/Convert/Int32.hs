module Unwitch.Convert.Int32
  ( toInt8
  , toInt16
  , toInt64
  , toInt
  , toInteger
  , toWord8
  , toWord16
  , toWord32
  , toWord64
  , toWord
  , toNatural
  , toFloat
  , toDouble
  , toInt8#
  , toInt16#
  , toInt#
  , toWord8#
  , toWord16#
  , toWord32#
  , toWord64#
  , toWord#
  , toNatural#
  , toFloat#
  )
where

import           Unwitch.Errors
import           Unwitch.Constant
import qualified Data.Bits as Bits
import           Data.Word
import           Data.Int
import           Numeric.Natural (Natural)
import           Prelude hiding (toInteger)

toInt8 :: Int32 -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Int32 -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt64 :: Int32 -> Int64
toInt64 = fromIntegral

toInt :: Int32 -> Maybe Int
toInt = Bits.toIntegralSized

toInteger :: Int32 -> Integer
toInteger = fromIntegral

toWord8 :: Int32 -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Int32 -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Int32 -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord64 :: Int32 -> Maybe Word64
toWord64 = Bits.toIntegralSized

toWord :: Int32 -> Maybe Word
toWord = Bits.toIntegralSized

toNatural :: Int32 -> Either Overflows Natural
toNatural x = if
  | x < 0     -> Left Underflow
  | otherwise  -> Right $ fromIntegral x

toFloat :: Int32 -> Either Overflows Float
toFloat x = if
  | x < -maxIntegralRepFloat -> Left Underflow
  | x > maxIntegralRepFloat  -> Left Overflow
  | otherwise                -> Right $ fromIntegral x

toDouble :: Int32 -> Double
toDouble = fromIntegral

toInt8# :: Int32 -> (# Int8 | (# #) #)
toInt8# x = case toInt8 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toInt16# :: Int32 -> (# Int16 | (# #) #)
toInt16# x = case toInt16 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toInt# :: Int32 -> (# Int | (# #) #)
toInt# x = case toInt x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord8# :: Int32 -> (# Word8 | (# #) #)
toWord8# x = case toWord8 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord16# :: Int32 -> (# Word16 | (# #) #)
toWord16# x = case toWord16 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord32# :: Int32 -> (# Word32 | (# #) #)
toWord32# x = case toWord32 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord64# :: Int32 -> (# Word64 | (# #) #)
toWord64# x = case toWord64 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord# :: Int32 -> (# Word | (# #) #)
toWord# x = case toWord x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toNatural# :: Int32 -> (# Overflows | Natural #)
toNatural# x = case toNatural x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toFloat# :: Int32 -> (# Overflows | Float #)
toFloat# x = case toFloat x of
  Left e  -> (# e | #)
  Right y -> (# | y #)
