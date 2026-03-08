module Unwitch.Convert.Int16
  ( toInt8
  , toInt32
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
  , toWord8#
  , toWord16#
  , toWord32#
  , toWord64#
  , toWord#
  , toNatural#
  )
where

import           Unwitch.Errors
import qualified Data.Bits as Bits
import           Data.Word
import           Data.Int
import           Numeric.Natural (Natural)
import           Prelude hiding (toInteger)

toInt8 :: Int16 -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt32 :: Int16 -> Int32
toInt32 = fromIntegral

toInt64 :: Int16 -> Int64
toInt64 = fromIntegral

toInt :: Int16 -> Int
toInt = fromIntegral

toInteger :: Int16 -> Integer
toInteger = fromIntegral

toWord8 :: Int16 -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Int16 -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Int16 -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord64 :: Int16 -> Maybe Word64
toWord64 = Bits.toIntegralSized

toWord :: Int16 -> Maybe Word
toWord = Bits.toIntegralSized

toNatural :: Int16 -> Either Overflows Natural
toNatural x = if
  | x < 0     -> Left Underflow
  | otherwise  -> Right $ fromIntegral x

toFloat :: Int16 -> Float
toFloat = fromIntegral

toDouble :: Int16 -> Double
toDouble = fromIntegral

toInt8# :: Int16 -> (# Int8 | (# #) #)
toInt8# x = case toInt8 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord8# :: Int16 -> (# Word8 | (# #) #)
toWord8# x = case toWord8 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord16# :: Int16 -> (# Word16 | (# #) #)
toWord16# x = case toWord16 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord32# :: Int16 -> (# Word32 | (# #) #)
toWord32# x = case toWord32 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord64# :: Int16 -> (# Word64 | (# #) #)
toWord64# x = case toWord64 x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toWord# :: Int16 -> (# Word | (# #) #)
toWord# x = case toWord x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

toNatural# :: Int16 -> (# Overflows | Natural #)
toNatural# x = case toNatural x of
  Left e  -> (# e | #)
  Right y -> (# | y #)
