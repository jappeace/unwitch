module Unwitch.Convert.Word32
  ( toWord8
  , toWord16
  , toWord64
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

toWord8 :: Word32 -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Word32 -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord64 :: Word32 -> Word64
toWord64 = fromIntegral

toWord :: Word32 -> Maybe Word
toWord = Bits.toIntegralSized

toNatural :: Word32 -> Natural
toNatural = fromIntegral

toInt8 :: Word32 -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Word32 -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt32 :: Word32 -> Maybe Int32
toInt32 = Bits.toIntegralSized

toInt64 :: Word32 -> Int64
toInt64 = fromIntegral

toInt :: Word32 -> Maybe Int
toInt = Bits.toIntegralSized

toInteger :: Word32 -> Integer
toInteger = fromIntegral

toFloat :: Word32 -> Either Overflows Float
toFloat x = if
  | x > maxIntegralRepFloat -> Left Overflow
  | otherwise               -> Right $ fromIntegral x

toDouble :: Word32 -> Double
toDouble = fromIntegral
