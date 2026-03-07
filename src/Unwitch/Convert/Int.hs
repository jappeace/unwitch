module Unwitch.Convert.Int
  ( toInt8
  , toInt16
  , toInt32
  , toInt64
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
