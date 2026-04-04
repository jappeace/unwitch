-- | Conversions from 'Word64'.
module Unwitch.Convert.Word64
  ( -- * Conversions
    toWord8
  , toWord16
  , toWord32
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
#ifdef __GLASGOW_HASKELL__
  , toCInt
#endif
#ifdef __GLASGOW_HASKELL__
  -- * Unboxed conversions
  -- $unboxed
  , toWord8#
  , toWord16#
  , toWord32#
  , toWord#
  , toInt8#
  , toInt16#
  , toInt32#
  , toInt64#
  , toInt#
  , toFloat#
  , toDouble#
#endif
  )
where

import           Unwitch.Errors
import           Unwitch.Constant
import qualified Data.Bits as Bits
import           Data.Word
import           Data.Int
import           Numeric.Natural (Natural)
import           Prelude hiding (toInteger)
#ifdef __GLASGOW_HASKELL__
import           Foreign.C.Types (CInt(CInt))
import           GHC.Exts (Int(..), Word(..), Float(..), Double(..),
                           word64ToWord#, wordToWord64#,
                           word2Int#,
                           wordToWord8#, word8ToWord#,
                           wordToWord16#, word16ToWord#,
                           wordToWord32#, word32ToWord#,
                           intToInt8#, intToInt16#, intToInt32#,
                           intToInt64#,
                           int2Float#, int2Double#,
                           eqWord64#, leWord64#,
                           (>=#))
import           GHC.Int (Int8(..), Int16(..), Int32(..), Int64(..))
import           GHC.Word (Word8(..), Word16(..), Word32(..), Word64(..))
#endif

#ifdef __GLASGOW_HASKELL__
-- $unboxed
-- These use GHC unboxed types and unboxed sums for zero-allocation
-- failure handling. Requires the @MagicHash@, @UnboxedSums@ and
-- @UnboxedTuples@ language extensions.
-- See the <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/primitives.html GHC manual on unboxed types>.
#endif

toWord8 :: Word64 -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Word64 -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Word64 -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord :: Word64 -> Maybe Word
toWord = Bits.toIntegralSized

toNatural :: Word64 -> Natural
toNatural = fromIntegral

toInt8 :: Word64 -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Word64 -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt32 :: Word64 -> Maybe Int32
toInt32 = Bits.toIntegralSized

toInt64 :: Word64 -> Maybe Int64
toInt64 = Bits.toIntegralSized

toInt :: Word64 -> Maybe Int
toInt = Bits.toIntegralSized

toInteger :: Word64 -> Integer
toInteger = fromIntegral

#ifdef __GLASGOW_HASKELL__
-- | Narrowing conversion via Int32, fails if outside Int32 range.
toCInt :: Word64 -> Maybe CInt
toCInt x = CInt <$> toInt32 x
#endif

-- | Checked conversion, fails with 'Overflow' if outside exact float integer range.
toFloat :: Word64 -> Either Overflows Float
toFloat x = if
  | x > maxIntegralRepFloat -> Left Overflow
  | otherwise               -> Right $ fromIntegral x

-- | Checked conversion, fails with 'Overflow' if outside exact double integer range.
toDouble :: Word64 -> Either Overflows Double
toDouble x = if
  | x > maxIntegralRepDouble -> Left Overflow
  | otherwise                -> Right $ fromIntegral x

#ifdef __GLASGOW_HASKELL__
-- | Unsigned narrowing via Word64# comparison
toWord8# :: Word64 -> (# Word8 | (# #) #)
toWord8# (W64# w64#) =
  let w# = word64ToWord# w64#
      n# = wordToWord8# w#
  in case eqWord64# (wordToWord64# (word8ToWord# n#)) w64# of
    1# -> (# W8# n# | #)
    _  -> (# | (# #) #)

-- | Unsigned narrowing via Word64# comparison
toWord16# :: Word64 -> (# Word16 | (# #) #)
toWord16# (W64# w64#) =
  let w# = word64ToWord# w64#
      n# = wordToWord16# w#
  in case eqWord64# (wordToWord64# (word16ToWord# n#)) w64# of
    1# -> (# W16# n# | #)
    _  -> (# | (# #) #)

-- | Unsigned narrowing via Word64# comparison
toWord32# :: Word64 -> (# Word32 | (# #) #)
toWord32# (W64# w64#) =
  let w# = word64ToWord# w64#
      n# = wordToWord32# w#
  in case eqWord64# (wordToWord64# (word32ToWord# n#)) w64# of
    1# -> (# W32# n# | #)
    _  -> (# | (# #) #)

-- | Roundtrip check at Word64# level
toWord# :: Word64 -> (# Word | (# #) #)
toWord# (W64# w64#) =
  let w# = word64ToWord# w64#
  in case eqWord64# (wordToWord64# w#) w64# of
    1# -> (# W# w# | #)
    _  -> (# | (# #) #)

-- | Check upper bound at Word64# level
toInt8# :: Word64 -> (# Int8 | (# #) #)
toInt8# (W64# w64#) = case leWord64# w64# (wordToWord64# 127##) of
  1# -> (# I8# (intToInt8# (word2Int# (word64ToWord# w64#))) | #)
  _  -> (# | (# #) #)

-- | Check upper bound for Int16
toInt16# :: Word64 -> (# Int16 | (# #) #)
toInt16# (W64# w64#) = case leWord64# w64# (wordToWord64# 32767##) of
  1# -> (# I16# (intToInt16# (word2Int# (word64ToWord# w64#))) | #)
  _  -> (# | (# #) #)

-- | Check upper bound for Int32
toInt32# :: Word64 -> (# Int32 | (# #) #)
toInt32# (W64# w64#) = case leWord64# w64# (wordToWord64# 2147483647##) of
  1# -> (# I32# (intToInt32# (word2Int# (word64ToWord# w64#))) | #)
  _  -> (# | (# #) #)

-- | Check high bit not set for Int64
toInt64# :: Word64 -> (# Int64 | (# #) #)
toInt64# (W64# w64#) =
  let w# = word64ToWord# w64#
      i# = word2Int# w#
  in case i# >=# 0# of
    1# -> case eqWord64# (wordToWord64# w#) w64# of
      1# -> (# I64# (intToInt64# i#) | #)
      _  -> (# | (# #) #)
    _  -> (# | (# #) #)

-- | Check fits in non-negative Int range
toInt# :: Word64 -> (# Int | (# #) #)
toInt# (W64# w64#) =
  let w# = word64ToWord# w64#
      i# = word2Int# w#
  in case i# >=# 0# of
    1# -> case eqWord64# (wordToWord64# w#) w64# of
      1# -> (# I# i# | #)
      _  -> (# | (# #) #)
    _  -> (# | (# #) #)

-- | Bounds-checked float conversion at Word64# level
toFloat# :: Word64 -> (# Overflows | Float #)
toFloat# (W64# w64#) = case leWord64# w64# (wordToWord64# 16777215##) of
  1# -> (# | F# (int2Float# (word2Int# (word64ToWord# w64#))) #)
  _  -> (# Overflow | #)

-- | Bounds-checked double conversion at Word64# level
toDouble# :: Word64 -> (# Overflows | Double #)
toDouble# (W64# w64#) = case leWord64# w64# (wordToWord64# 9007199254740991##) of
  1# -> (# | D# (int2Double# (word2Int# (word64ToWord# w64#))) #)
  _  -> (# Overflow | #)
#endif
