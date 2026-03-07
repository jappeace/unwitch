module Unwitch.Convert.Natural
  ( toWord8
  , toWord16
  , toWord32
  , toWord64
  , toWord
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

toWord8 :: Natural -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Natural -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Natural -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord64 :: Natural -> Maybe Word64
toWord64 = Bits.toIntegralSized

toWord :: Natural -> Maybe Word
toWord = Bits.toIntegralSized

toInt8 :: Natural -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Natural -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt32 :: Natural -> Maybe Int32
toInt32 = Bits.toIntegralSized

toInt64 :: Natural -> Maybe Int64
toInt64 = Bits.toIntegralSized

toInt :: Natural -> Maybe Int
toInt = Bits.toIntegralSized

toInteger :: Natural -> Integer
toInteger = fromIntegral

toFloat :: Natural -> Either Overflows Float
toFloat x = if
  | x > maxIntegralRepFloat -> Left Overflow
  | otherwise               -> Right $ fromIntegral x

toDouble :: Natural -> Either Overflows Double
toDouble x = if
  | x > maxIntegralRepDouble -> Left Overflow
  | otherwise                -> Right $ fromIntegral x
