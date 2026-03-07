module Unwitch.Convert.Int64
  ( toInt8
  , toInt16
  , toInt32
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
  )
where

import           Unwitch.Errors
import           Unwitch.Constant
import qualified Data.Bits as Bits
import           Data.Word
import           Data.Int
import           Numeric.Natural (Natural)
import           Prelude hiding (toInteger)

toInt8 :: Int64 -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Int64 -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt32 :: Int64 -> Maybe Int32
toInt32 = Bits.toIntegralSized

toInt :: Int64 -> Maybe Int
toInt = Bits.toIntegralSized

toInteger :: Int64 -> Integer
toInteger = fromIntegral

toWord8 :: Int64 -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Int64 -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Int64 -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord64 :: Int64 -> Maybe Word64
toWord64 = Bits.toIntegralSized

toWord :: Int64 -> Maybe Word
toWord = Bits.toIntegralSized

toNatural :: Int64 -> Either Overflows Natural
toNatural x = if
  | x < 0     -> Left Underflow
  | otherwise  -> Right $ fromIntegral x

toFloat :: Int64 -> Either Overflows Float
toFloat x = if
  | x < -maxIntegralRepFloat -> Left Underflow
  | x > maxIntegralRepFloat  -> Left Overflow
  | otherwise                -> Right $ fromIntegral x

toDouble :: Int64 -> Either Overflows Double
toDouble x = if
  | x < -maxIntegralRepDouble -> Left Underflow
  | x > maxIntegralRepDouble  -> Left Overflow
  | otherwise                 -> Right $ fromIntegral x
