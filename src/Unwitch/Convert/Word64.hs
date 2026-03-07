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
