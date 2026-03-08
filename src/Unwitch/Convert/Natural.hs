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
  , toWord8#
  , toWord16#
  , toWord32#
  , toWord64#
  , toWord#
  , toInt8#
  , toInt16#
  , toInt32#
  , toInt64#
  , toInt#
  , toFloat#
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
                           word2Int#,
                           wordToWord8#, word8ToWord#,
                           wordToWord16#, word16ToWord#,
                           wordToWord32#, word32ToWord#,
                           wordToWord64#,
                           intToInt8#,
                           intToInt16#,
                           intToInt32#,
                           intToInt64#,
                           int2Float#, int2Double#,
                           eqWord#, leWord#, (>=#))
import           GHC.Int (Int8(..), Int16(..), Int32(..), Int64(..))
import           GHC.Word (Word8(..), Word16(..), Word32(..), Word64(..))
import           GHC.Num.Natural (naturalToWordMaybe#)

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

-- | Via naturalToWordMaybe#, then narrow and roundtrip at Word#
toWord8# :: Natural -> (# Word8 | (# #) #)
toWord8# nat = case naturalToWordMaybe# nat of
  (# (# #) | #) -> (# | (# #) #)
  (# | w# #) ->
    let n# = wordToWord8# w#
    in case word8ToWord# n# `eqWord#` w# of
      1# -> (# W8# n# | #)
      _  -> (# | (# #) #)

-- | Via naturalToWordMaybe#, then narrow
toWord16# :: Natural -> (# Word16 | (# #) #)
toWord16# nat = case naturalToWordMaybe# nat of
  (# (# #) | #) -> (# | (# #) #)
  (# | w# #) ->
    let n# = wordToWord16# w#
    in case word16ToWord# n# `eqWord#` w# of
      1# -> (# W16# n# | #)
      _  -> (# | (# #) #)

-- | Via naturalToWordMaybe#, then narrow
toWord32# :: Natural -> (# Word32 | (# #) #)
toWord32# nat = case naturalToWordMaybe# nat of
  (# (# #) | #) -> (# | (# #) #)
  (# | w# #) ->
    let n# = wordToWord32# w#
    in case word32ToWord# n# `eqWord#` w# of
      1# -> (# W32# n# | #)
      _  -> (# | (# #) #)

-- | Via naturalToWordMaybe#, then widen to Word64
toWord64# :: Natural -> (# Word64 | (# #) #)
toWord64# nat = case naturalToWordMaybe# nat of
  (# (# #) | #) -> (# | (# #) #)
  (# | w# #) -> (# W64# (wordToWord64# w#) | #)

-- | Via naturalToWordMaybe#
toWord# :: Natural -> (# Word | (# #) #)
toWord# nat = case naturalToWordMaybe# nat of
  (# (# #) | #) -> (# | (# #) #)
  (# | w# #) -> (# W# w# | #)

-- | Via naturalToWordMaybe#, check upper bound for Int8
toInt8# :: Natural -> (# Int8 | (# #) #)
toInt8# nat = case naturalToWordMaybe# nat of
  (# (# #) | #) -> (# | (# #) #)
  (# | w# #) -> case leWord# w# 127## of
    1# -> (# I8# (intToInt8# (word2Int# w#)) | #)
    _  -> (# | (# #) #)

-- | Via naturalToWordMaybe#, check upper bound for Int16
toInt16# :: Natural -> (# Int16 | (# #) #)
toInt16# nat = case naturalToWordMaybe# nat of
  (# (# #) | #) -> (# | (# #) #)
  (# | w# #) -> case leWord# w# 32767## of
    1# -> (# I16# (intToInt16# (word2Int# w#)) | #)
    _  -> (# | (# #) #)

-- | Via naturalToWordMaybe#, check upper bound for Int32
toInt32# :: Natural -> (# Int32 | (# #) #)
toInt32# nat = case naturalToWordMaybe# nat of
  (# (# #) | #) -> (# | (# #) #)
  (# | w# #) -> case leWord# w# 2147483647## of
    1# -> (# I32# (intToInt32# (word2Int# w#)) | #)
    _  -> (# | (# #) #)

-- | Via naturalToWordMaybe#, check fits in non-negative Int64
toInt64# :: Natural -> (# Int64 | (# #) #)
toInt64# nat = case naturalToWordMaybe# nat of
  (# (# #) | #) -> (# | (# #) #)
  (# | w# #) ->
    let i# = word2Int# w#
    in case i# >=# 0# of
      1# -> (# I64# (intToInt64# i#) | #)
      _  -> (# | (# #) #)

-- | Via naturalToWordMaybe#, check fits in non-negative Int
toInt# :: Natural -> (# Int | (# #) #)
toInt# nat = case naturalToWordMaybe# nat of
  (# (# #) | #) -> (# | (# #) #)
  (# | w# #) ->
    let i# = word2Int# w#
    in case i# >=# 0# of
      1# -> (# I# i# | #)
      _  -> (# | (# #) #)

-- | Via naturalToWordMaybe#, bounds-checked float
toFloat# :: Natural -> (# Overflows | Float #)
toFloat# nat = case naturalToWordMaybe# nat of
  (# (# #) | #) -> (# Overflow | #)
  (# | w# #) -> case leWord# w# 16777215## of
    1# -> (# | F# (int2Float# (word2Int# w#)) #)
    _  -> (# Overflow | #)

-- | Via naturalToWordMaybe#, bounds-checked double
toDouble# :: Natural -> (# Overflows | Double #)
toDouble# nat = case naturalToWordMaybe# nat of
  (# (# #) | #) -> (# Overflow | #)
  (# | w# #) -> case leWord# w# 9007199254740991## of
    1# -> (# | D# (int2Double# (word2Int# w#)) #)
    _  -> (# Overflow | #)
