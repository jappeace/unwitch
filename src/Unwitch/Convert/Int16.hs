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
  , toInt8#
  , toWord8#
  , toWord16#
  , toWord32#
  , toWord64#
  , toWord#
  , toNatural#
  )
where

import           Unwitch.Errors
import qualified Data.Bits as Bits
import           Data.Word
import           Data.Int
import           Numeric.Natural (Natural)
import           Prelude hiding (toInteger)
import           GHC.Exts (Word(..), int16ToInt#, intToInt8#, int8ToInt#,
                           int2Word#, word2Int#,
                           wordToWord8#, word8ToWord#,
                           wordToWord16#, wordToWord32#, wordToWord64#,
                           (==#), (>=#))
import           GHC.Int (Int8(..), Int16(..))
import           GHC.Word (Word8(..), Word16(..), Word32(..), Word64(..))
import           GHC.Num.Natural (Natural(NS))

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

-- | Signed narrowing, roundtrip at Int#
toInt8# :: Int16 -> (# Int8 | (# #) #)
toInt8# (I16# x16#) =
  let i# = int16ToInt# x16#
      n# = intToInt8# i#
  in case int8ToInt# n# ==# i# of
    1# -> (# I8# n# | #)
    _  -> (# | (# #) #)

-- | Signed->unsigned narrow, roundtrip via Word# back to Int#
toWord8# :: Int16 -> (# Word8 | (# #) #)
toWord8# (I16# x16#) =
  let i# = int16ToInt# x16#
      n# = wordToWord8# (int2Word# i#)
  in case word2Int# (word8ToWord# n#) ==# i# of
    1# -> (# W8# n# | #)
    _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative
toWord16# :: Int16 -> (# Word16 | (# #) #)
toWord16# (I16# x16#) = case int16ToInt# x16# >=# 0# of
  1# -> (# W16# (wordToWord16# (int2Word# (int16ToInt# x16#))) | #)
  _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative
toWord32# :: Int16 -> (# Word32 | (# #) #)
toWord32# (I16# x16#) = case int16ToInt# x16# >=# 0# of
  1# -> (# W32# (wordToWord32# (int2Word# (int16ToInt# x16#))) | #)
  _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative
toWord64# :: Int16 -> (# Word64 | (# #) #)
toWord64# (I16# x16#) = case int16ToInt# x16# >=# 0# of
  1# -> (# W64# (wordToWord64# (int2Word# (int16ToInt# x16#))) | #)
  _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative
toWord# :: Int16 -> (# Word | (# #) #)
toWord# (I16# x16#) = case int16ToInt# x16# >=# 0# of
  1# -> (# W# (int2Word# (int16ToInt# x16#)) | #)
  _  -> (# | (# #) #)

-- | Check non-negative, construct NS directly
toNatural# :: Int16 -> (# Overflows | Natural #)
toNatural# (I16# x16#) = case int16ToInt# x16# >=# 0# of
  1# -> (# | NS (int2Word# (int16ToInt# x16#)) #)
  _  -> (# Underflow | #)
