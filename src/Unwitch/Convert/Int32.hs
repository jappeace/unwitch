-- | Conversions from 'Int32'.
module Unwitch.Convert.Int32
  ( -- * Conversions
    toInt8
  , toInt16
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
  , toCInt
  -- * Unboxed conversions
  -- $unboxed
  , toInt8#
  , toInt16#
  , toInt#
  , toWord8#
  , toWord16#
  , toWord32#
  , toWord64#
  , toWord#
  , toNatural#
  , toFloat#
  )
where

import           Unwitch.Errors
import           Unwitch.Constant
import qualified Data.Bits as Bits
import           Data.Word
import           Data.Int
import           Numeric.Natural (Natural)
import           Foreign.C.Types (CInt(CInt))
import           Prelude hiding (toInteger)
import           GHC.Exts (Int(..), Word(..), Float(..),
                           int32ToInt#, intToInt8#, int8ToInt#,
                           intToInt16#, int16ToInt#,
                           int2Word#, word2Int#,
                           wordToWord8#, word8ToWord#,
                           wordToWord16#, word16ToWord#,
                           wordToWord32#, wordToWord64#,
                           int2Float#,
                           (==#), (>=#), (<#), (>#))
import           GHC.Int (Int8(..), Int16(..), Int32(..))
import           GHC.Word (Word8(..), Word16(..), Word32(..), Word64(..))
import           GHC.Num.Natural (Natural(NS))

-- $unboxed
-- These use GHC unboxed types and unboxed sums for zero-allocation
-- failure handling. Requires the @MagicHash@, @UnboxedSums@ and
-- @UnboxedTuples@ language extensions.
-- See the <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/primitives.html GHC manual on unboxed types>.

toInt8 :: Int32 -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Int32 -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt64 :: Int32 -> Int64
toInt64 = fromIntegral

toInt :: Int32 -> Maybe Int
toInt = Bits.toIntegralSized

toInteger :: Int32 -> Integer
toInteger = fromIntegral

toWord8 :: Int32 -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Int32 -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Int32 -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord64 :: Int32 -> Maybe Word64
toWord64 = Bits.toIntegralSized

toWord :: Int32 -> Maybe Word
toWord = Bits.toIntegralSized

-- | Signed-to-unsigned conversion, returns 'Left' 'Underflow' for negative values.
toNatural :: Int32 -> Either Overflows Natural
toNatural x = if
  | x < 0     -> Left Underflow
  | otherwise  -> Right $ fromIntegral x

-- | Checked conversion, fails if outside exact float integer range (+-16777215).
toFloat :: Int32 -> Either Overflows Float
toFloat x = if
  | x < -maxIntegralRepFloat -> Left Underflow
  | x > maxIntegralRepFloat  -> Left Overflow
  | otherwise                -> Right $ fromIntegral x

-- | Direct wrapping, CInt is a newtype over Int32.
toCInt :: Int32 -> CInt
toCInt = CInt

toDouble :: Int32 -> Double
toDouble = fromIntegral

-- | Signed narrowing, roundtrip at Int#
toInt8# :: Int32 -> (# Int8 | (# #) #)
toInt8# (I32# x32#) =
  let i# = int32ToInt# x32#
      n# = intToInt8# i#
  in case int8ToInt# n# ==# i# of
    1# -> (# I8# n# | #)
    _  -> (# | (# #) #)

-- | Signed narrowing, roundtrip at Int#
toInt16# :: Int32 -> (# Int16 | (# #) #)
toInt16# (I32# x32#) =
  let i# = int32ToInt# x32#
      n# = intToInt16# i#
  in case int16ToInt# n# ==# i# of
    1# -> (# I16# n# | #)
    _  -> (# | (# #) #)

-- | Int32 always fits in Int (Int is at least 32 bits)
toInt# :: Int32 -> (# Int | (# #) #)
toInt# (I32# x32#) = (# I# (int32ToInt# x32#) | #)

-- | Signed->unsigned narrow, roundtrip via Word# back to Int#
toWord8# :: Int32 -> (# Word8 | (# #) #)
toWord8# (I32# x32#) =
  let i# = int32ToInt# x32#
      n# = wordToWord8# (int2Word# i#)
  in case word2Int# (word8ToWord# n#) ==# i# of
    1# -> (# W8# n# | #)
    _  -> (# | (# #) #)

-- | Signed->unsigned narrow, roundtrip via Word# back to Int#
toWord16# :: Int32 -> (# Word16 | (# #) #)
toWord16# (I32# x32#) =
  let i# = int32ToInt# x32#
      n# = wordToWord16# (int2Word# i#)
  in case word2Int# (word16ToWord# n#) ==# i# of
    1# -> (# W16# n# | #)
    _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative
toWord32# :: Int32 -> (# Word32 | (# #) #)
toWord32# (I32# x32#) = case int32ToInt# x32# >=# 0# of
  1# -> (# W32# (wordToWord32# (int2Word# (int32ToInt# x32#))) | #)
  _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative
toWord64# :: Int32 -> (# Word64 | (# #) #)
toWord64# (I32# x32#) = case int32ToInt# x32# >=# 0# of
  1# -> (# W64# (wordToWord64# (int2Word# (int32ToInt# x32#))) | #)
  _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative
toWord# :: Int32 -> (# Word | (# #) #)
toWord# (I32# x32#) = case int32ToInt# x32# >=# 0# of
  1# -> (# W# (int2Word# (int32ToInt# x32#)) | #)
  _  -> (# | (# #) #)

-- | Check non-negative, construct NS directly
toNatural# :: Int32 -> (# Overflows | Natural #)
toNatural# (I32# x32#) = case int32ToInt# x32# >=# 0# of
  1# -> (# | NS (int2Word# (int32ToInt# x32#)) #)
  _  -> (# Underflow | #)

-- | Bounds-checked float conversion
toFloat# :: Int32 -> (# Overflows | Float #)
toFloat# (I32# x32#) =
  let i# = int32ToInt# x32#
  in case i# <# -16777215# of
    1# -> (# Underflow | #)
    _  -> case i# ># 16777215# of
      1# -> (# Overflow | #)
      _  -> (# | F# (int2Float# i#) #)
