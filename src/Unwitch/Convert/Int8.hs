module Unwitch.Convert.Int8
  ( toInt16
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

toInt16 :: Int8 -> Int16
toInt16 = fromIntegral

toInt32 :: Int8 -> Int32
toInt32 = fromIntegral

toInt64 :: Int8 -> Int64
toInt64 = fromIntegral

toInt :: Int8 -> Int
toInt = fromIntegral

toInteger :: Int8 -> Integer
toInteger = fromIntegral

toWord8 :: Int8 -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Int8 -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Int8 -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord64 :: Int8 -> Maybe Word64
toWord64 = Bits.toIntegralSized

toWord :: Int8 -> Maybe Word
toWord = Bits.toIntegralSized

toNatural :: Int8 -> Either Overflows Natural
toNatural x = if
  | x < 0     -> Left Underflow
  | otherwise  -> Right $ fromIntegral x

toFloat :: Int8 -> Float
toFloat = fromIntegral

toDouble :: Int8 -> Double
toDouble = fromIntegral
