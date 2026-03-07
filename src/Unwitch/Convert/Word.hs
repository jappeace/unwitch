module Unwitch.Convert.Word
  ( toWord8
  , toWord16
  , toWord32
  , toWord64
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

toWord8 :: Word -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Word -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Word -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord64 :: Word -> Word64
toWord64 = fromIntegral

toNatural :: Word -> Natural
toNatural = fromIntegral

toInt8 :: Word -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Word -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt32 :: Word -> Maybe Int32
toInt32 = Bits.toIntegralSized

toInt64 :: Word -> Maybe Int64
toInt64 = Bits.toIntegralSized

toInt :: Word -> Maybe Int
toInt = Bits.toIntegralSized

toInteger :: Word -> Integer
toInteger = fromIntegral

toFloat :: Word -> Either Overflows Float
toFloat x = if
  | x > maxIntegralRepFloat -> Left Overflow
  | otherwise               -> Right $ fromIntegral x

toDouble :: Word -> Either Overflows Double
toDouble x = if
  | fromIntegral x > (maxIntegralRepDouble :: Integer) -> Left Overflow
  | otherwise                                          -> Right $ fromIntegral x
