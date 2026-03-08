module Unwitch.Convert.Int
  ( toInt8
  , toInt8#
  , toInt16
  , toInt16#
  , toInt32
  , toInt32#
  , toInt64
  , toInteger
  , toWord8
  , toWord8#
  , toWord16
  , toWord16#
  , toWord32
  , toWord32#
  , toWord64
  , toWord64#
  , toWord
  , toWord#
  , toNatural
  , toNatural#
  , toFloat
  , toFloat#
  , toDouble
  , toDouble#
  )
where

import           Unwitch.Errors
import           Unwitch.Constant
import qualified Data.Bits as Bits
import           Data.Word
import           Data.Int
import           Numeric.Natural (Natural)
import           Prelude hiding (toInteger)
import           GHC.Exts (Int(..), Word(..), Float(..), Double(..),
                           intToInt8#, int8ToInt#, intToInt16#, int16ToInt#,
                           intToInt32#, int32ToInt#,
                           int2Word#, wordToWord8#, word8ToWord#,
                           wordToWord16#, word16ToWord#,
                           wordToWord32#, word32ToWord#,
                           wordToWord64#,
                           int2Float#, int2Double#,
                           (==#), (>=#), (<#), (>#),
                           word2Int#)
import           GHC.Int (Int8(..), Int16(..), Int32(..))
import           GHC.Word (Word8(..), Word16(..), Word32(..), Word64(..))
import           GHC.Num.Natural (Natural(NS))

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

-- | Signed narrowing, roundtrip at Int#
toInt8# :: Int -> (# Int8 | (# #) #)
toInt8# (I# x#) =
  let n# = intToInt8# x#
  in case int8ToInt# n# ==# x# of
    1# -> (# I8# n# | #)
    _  -> (# | (# #) #)

-- | Signed narrowing, roundtrip at Int#
toInt16# :: Int -> (# Int16 | (# #) #)
toInt16# (I# x#) =
  let n# = intToInt16# x#
  in case int16ToInt# n# ==# x# of
    1# -> (# I16# n# | #)
    _  -> (# | (# #) #)

-- | Signed narrowing, roundtrip at Int#
toInt32# :: Int -> (# Int32 | (# #) #)
toInt32# (I# x#) =
  let n# = intToInt32# x#
  in case int32ToInt# n# ==# x# of
    1# -> (# I32# n# | #)
    _  -> (# | (# #) #)

-- | Signed->unsigned narrow, roundtrip via Word# back to Int#
toWord8# :: Int -> (# Word8 | (# #) #)
toWord8# (I# x#) =
  let n# = wordToWord8# (int2Word# x#)
  in case word2Int# (word8ToWord# n#) ==# x# of
    1# -> (# W8# n# | #)
    _  -> (# | (# #) #)

-- | Signed->unsigned narrow, roundtrip via Word# back to Int#
toWord16# :: Int -> (# Word16 | (# #) #)
toWord16# (I# x#) =
  let n# = wordToWord16# (int2Word# x#)
  in case word2Int# (word16ToWord# n#) ==# x# of
    1# -> (# W16# n# | #)
    _  -> (# | (# #) #)

-- | Signed->unsigned narrow, roundtrip via Word# back to Int#
toWord32# :: Int -> (# Word32 | (# #) #)
toWord32# (I# x#) =
  let n# = wordToWord32# (int2Word# x#)
  in case word2Int# (word32ToWord# n#) ==# x# of
    1# -> (# W32# n# | #)
    _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative
toWord64# :: Int -> (# Word64 | (# #) #)
toWord64# (I# x#) = case x# >=# 0# of
  1# -> (# W64# (wordToWord64# (int2Word# x#)) | #)
  _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative
toWord# :: Int -> (# Word | (# #) #)
toWord# (I# x#) = case x# >=# 0# of
  1# -> (# W# (int2Word# x#) | #)
  _  -> (# | (# #) #)

-- | Check non-negative, construct NS directly
toNatural# :: Int -> (# Overflows | Natural #)
toNatural# (I# i#) = case i# >=# 0# of
  1# -> (# | NS (int2Word# i#) #)
  _  -> (# Underflow | #)

-- | Bounds-checked float conversion
toFloat# :: Int -> (# Overflows | Float #)
toFloat# (I# i#) = case i# <# -16777215# of
  1# -> (# Underflow | #)
  _  -> case i# ># 16777215# of
    1# -> (# Overflow | #)
    _  -> (# | F# (int2Float# i#) #)

-- | Bounds-checked double conversion
toDouble# :: Int -> (# Overflows | Double #)
toDouble# (I# i#) = case i# <# -9007199254740991# of
  1# -> (# Underflow | #)
  _  -> case i# ># 9007199254740991# of
    1# -> (# Overflow | #)
    _  -> (# | D# (int2Double# i#) #)
