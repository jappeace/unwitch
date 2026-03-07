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
