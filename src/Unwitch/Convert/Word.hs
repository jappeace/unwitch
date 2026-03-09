module Unwitch.Convert.Word
  ( -- * Conversions
    toWord8
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
  -- * Unboxed conversions
  -- $unboxed
  , toWord8#
  , toWord16#
  , toWord32#
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
                           wordToWord8#, word8ToWord#,
                           wordToWord16#, word16ToWord#,
                           wordToWord32#, word32ToWord#,
                           word2Int#,
                           intToInt8#, intToInt16#, intToInt32#,
                           intToInt64#,
                           int2Float#, int2Double#,
                           eqWord#, leWord#, (>=#))
import           GHC.Int (Int8(..), Int16(..), Int32(..), Int64(..))
import           GHC.Word (Word8(..), Word16(..), Word32(..))

-- $unboxed
-- These use GHC unboxed types and unboxed sums for zero-allocation
-- failure handling. Requires the @MagicHash@, @UnboxedSums@ and
-- @UnboxedTuples@ language extensions.
-- See the <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/primitives.html GHC manual on unboxed types>.

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

-- | Unsigned narrowing, roundtrip at Word#
toWord8# :: Word -> (# Word8 | (# #) #)
toWord8# (W# w#) =
  let n# = wordToWord8# w#
  in case word8ToWord# n# `eqWord#` w# of
    1# -> (# W8# n# | #)
    _  -> (# | (# #) #)

-- | Unsigned narrowing, roundtrip at Word#
toWord16# :: Word -> (# Word16 | (# #) #)
toWord16# (W# w#) =
  let n# = wordToWord16# w#
  in case word16ToWord# n# `eqWord#` w# of
    1# -> (# W16# n# | #)
    _  -> (# | (# #) #)

-- | Unsigned narrowing, roundtrip at Word#
toWord32# :: Word -> (# Word32 | (# #) #)
toWord32# (W# w#) =
  let n# = wordToWord32# w#
  in case word32ToWord# n# `eqWord#` w# of
    1# -> (# W32# n# | #)
    _  -> (# | (# #) #)

-- | Check upper bound for signed target
toInt8# :: Word -> (# Int8 | (# #) #)
toInt8# (W# w#) = case leWord# w# 127## of
  1# -> (# I8# (intToInt8# (word2Int# w#)) | #)
  _  -> (# | (# #) #)

-- | Check upper bound for signed target
toInt16# :: Word -> (# Int16 | (# #) #)
toInt16# (W# w#) = case leWord# w# 32767## of
  1# -> (# I16# (intToInt16# (word2Int# w#)) | #)
  _  -> (# | (# #) #)

-- | Check upper bound for signed target
toInt32# :: Word -> (# Int32 | (# #) #)
toInt32# (W# w#) = case leWord# w# 2147483647## of
  1# -> (# I32# (intToInt32# (word2Int# w#)) | #)
  _  -> (# | (# #) #)

-- | Check high bit not set for Int64
toInt64# :: Word -> (# Int64 | (# #) #)
toInt64# (W# w#) =
  let i# = word2Int# w#
  in case i# >=# 0# of
    1# -> (# I64# (intToInt64# i#) | #)
    _  -> (# | (# #) #)

-- | Check high bit not set for Int
toInt# :: Word -> (# Int | (# #) #)
toInt# (W# w#) =
  let i# = word2Int# w#
  in case i# >=# 0# of
    1# -> (# I# i# | #)
    _  -> (# | (# #) #)

-- | Bounds-checked float conversion
toFloat# :: Word -> (# Overflows | Float #)
toFloat# (W# w#) = case leWord# w# 16777215## of
  1# -> (# | F# (int2Float# (word2Int# w#)) #)
  _  -> (# Overflow | #)

-- | Bounds-checked double conversion
toDouble# :: Word -> (# Overflows | Double #)
toDouble# (W# w#) = case leWord# w# 9007199254740991## of
  1# -> (# | D# (int2Double# (word2Int# w#)) #)
  _  -> (# Overflow | #)
