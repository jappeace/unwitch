module Unwitch.Convert.Int
  ( toInt8
  , toInt8#
  , toInt16
  , toInt16#
  , toInt32
  , toInt32#
  , toInt64
  , toInteger
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
  , toNatural
  , toNatural#
  , toFloat
  , toFloat#
  , toDouble
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

toInt8 :: Int -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Int -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt32 :: Int -> Maybe Int32
toInt32 = Bits.toIntegralSized

toInt64 :: Int -> Int64
toInt64 = fromIntegral

toInteger :: Int -> Integer
toInteger = fromIntegral

toWord8 :: Int -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Int -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Int -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord64 :: Int -> Maybe Word64
toWord64 = Bits.toIntegralSized

toWord :: Int -> Maybe Word
toWord = Bits.toIntegralSized

toNatural :: Int -> Either Overflows Natural
toNatural x = if
  | x < 0     -> Left Underflow
  | otherwise  -> Right $ fromIntegral x

toFloat :: Int -> Either Overflows Float
toFloat x = if
  | x < -maxIntegralRepFloat -> Left Underflow
  | x > maxIntegralRepFloat  -> Left Overflow
  | otherwise                -> Right $ fromIntegral x

toDouble :: Int -> Either Overflows Double
toDouble x = if
  | fromIntegral x < (-maxIntegralRepDouble :: Integer) -> Left Underflow
  | fromIntegral x > (maxIntegralRepDouble :: Integer)  -> Left Overflow
  | otherwise                                           -> Right $ fromIntegral x

toInt8# :: Int -> (# Int8 | (# #) #)
toInt8# x = case toInt8 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toInt16# :: Int -> (# Int16 | (# #) #)
toInt16# x = case toInt16 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toInt32# :: Int -> (# Int32 | (# #) #)
toInt32# x = case toInt32 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord8# :: Int -> (# Word8 | (# #) #)
toWord8# x = case toWord8 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord16# :: Int -> (# Word16 | (# #) #)
toWord16# x = case toWord16 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord32# :: Int -> (# Word32 | (# #) #)
toWord32# x = case toWord32 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord64# :: Int -> (# Word64 | (# #) #)
toWord64# x = case toWord64 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord# :: Int -> (# Word | (# #) #)
toWord# x = case toWord x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toNatural# :: Int -> (# Overflows | Natural #)
toNatural# x = case toNatural x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toFloat# :: Int -> (# Overflows | Float #)
toFloat# x = case toFloat x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toDouble# :: Int -> (# Overflows | Double #)
toDouble# x = case toDouble x of
  Left e  -> (# e | #)
  Right y -> (# | y #)
