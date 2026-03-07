module Unwitch.Convert.Word16
  ( toWord8
  , toWord32
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

import qualified Data.Bits as Bits
import           Data.Word
import           Data.Int
import           Numeric.Natural (Natural)
import           Prelude hiding (toInteger)

toWord8 :: Word16 -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord32 :: Word16 -> Word32
toWord32 = fromIntegral

toWord64 :: Word16 -> Word64
toWord64 = fromIntegral

toWord :: Word16 -> Word
toWord = fromIntegral

toNatural :: Word16 -> Natural
toNatural = fromIntegral

toInt8 :: Word16 -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Word16 -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt32 :: Word16 -> Int32
toInt32 = fromIntegral

toInt64 :: Word16 -> Int64
toInt64 = fromIntegral

toInt :: Word16 -> Int
toInt = fromIntegral

toInteger :: Word16 -> Integer
toInteger = fromIntegral

toFloat :: Word16 -> Float
toFloat = fromIntegral

toDouble :: Word16 -> Double
toDouble = fromIntegral
