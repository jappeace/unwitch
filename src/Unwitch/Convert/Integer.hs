module Unwitch.Convert.Integer
  ( toDouble
  , toInt8
  , toInt16
  , toInt32
  , toInt64
  , toInt
  , toWord8
  , toWord16
  , toWord32
  , toWord64
  , toWord
  )
where

import           Unwitch.Errors
import           Unwitch.Constant
import qualified Data.Bits as Bits
import Data.Word
import Data.Int


toDouble :: Integer -> Either Overflows Double
toDouble integer = if
    | integer < -maxIntegralRepDouble -> Left Underflow
    | integer > maxIntegralRepDouble -> Left Overflow
    | otherwise -> Right $ Prelude.fromIntegral integer

toInt8 :: Integer -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Integer -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt32 :: Integer -> Maybe Int32
toInt32 = Bits.toIntegralSized

toInt64 :: Integer -> Maybe Int64
toInt64 = Bits.toIntegralSized

toInt :: Integer -> Maybe Int
toInt = Bits.toIntegralSized

toWord8 :: Integer -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Integer -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Integer -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord64 :: Integer -> Maybe Word64
toWord64 = Bits.toIntegralSized

toWord :: Integer -> Maybe Word
toWord = Bits.toIntegralSized
