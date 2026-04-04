-- | Conversions from 'Word32'.
module Unwitch.Convert.Word32
  ( -- * Conversions
    toWord8
  , toWord16
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
#ifdef __GLASGOW_HASKELL__
  , toCInt
#endif
#ifdef __GLASGOW_HASKELL__
  -- * Unboxed conversions
  -- $unboxed
  , toWord8#
  , toWord16#
  , toWord#
  , toInt8#
  , toInt16#
  , toInt32#
  , toInt#
  , toFloat#
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
import           GHC.Exts (Int(..), Word(..), Float(..),
                           word32ToWord#, word2Int#,
                           wordToWord8#, word8ToWord#,
                           wordToWord16#, word16ToWord#,
                           intToInt8#, int8ToInt#,
                           intToInt16#, int16ToInt#,
                           intToInt32#, int32ToInt#,
                           int2Float#,
                           eqWord#, leWord#, (==#), (>=#))
import           GHC.Int (Int8(..), Int16(..), Int32(..))
import           GHC.Word (Word8(..), Word16(..), Word32(..))
#endif

#ifdef __GLASGOW_HASKELL__
-- $unboxed
-- These use GHC unboxed types and unboxed sums for zero-allocation
-- failure handling. Requires the @MagicHash@, @UnboxedSums@ and
-- @UnboxedTuples@ language extensions.
-- See the <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/primitives.html GHC manual on unboxed types>.
#endif

toWord8 :: Word32 -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Word32 -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord64 :: Word32 -> Word64
toWord64 = fromIntegral

toWord :: Word32 -> Maybe Word
toWord = Bits.toIntegralSized

toNatural :: Word32 -> Natural
toNatural = fromIntegral

toInt8 :: Word32 -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Word32 -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt32 :: Word32 -> Maybe Int32
toInt32 = Bits.toIntegralSized

toInt64 :: Word32 -> Int64
toInt64 = fromIntegral

toInt :: Word32 -> Maybe Int
toInt = Bits.toIntegralSized

toInteger :: Word32 -> Integer
toInteger = fromIntegral

-- | Checked conversion, fails with 'Overflow' if outside exact float integer range.
toFloat :: Word32 -> Either Overflows Float
toFloat x = if
  | x > maxIntegralRepFloat -> Left Overflow
  | otherwise               -> Right $ fromIntegral x

toDouble :: Word32 -> Double
toDouble = fromIntegral

#ifdef __GLASGOW_HASKELL__
-- | Narrowing conversion via Int32, fails if outside Int32 range.
toCInt :: Word32 -> Maybe CInt
toCInt x = CInt <$> toInt32 x
#endif

#ifdef __GLASGOW_HASKELL__
-- | Unsigned narrowing, roundtrip at Word#
toWord8# :: Word32 -> (# Word8 | (# #) #)
toWord8# (W32# w32#) =
  let w# = word32ToWord# w32#
      n# = wordToWord8# w#
  in case word8ToWord# n# `eqWord#` w# of
    1# -> (# W8# n# | #)
    _  -> (# | (# #) #)

-- | Unsigned narrowing, roundtrip at Word#
toWord16# :: Word32 -> (# Word16 | (# #) #)
toWord16# (W32# w32#) =
  let w# = word32ToWord# w32#
      n# = wordToWord16# w#
  in case word16ToWord# n# `eqWord#` w# of
    1# -> (# W16# n# | #)
    _  -> (# | (# #) #)

-- | Word32 always fits in Word (Word is at least 32 bits)
toWord# :: Word32 -> (# Word | (# #) #)
toWord# (W32# w32#) = (# W# (word32ToWord# w32#) | #)

-- | Unsigned->signed, source fits in Int#, roundtrip at Int#
toInt8# :: Word32 -> (# Int8 | (# #) #)
toInt8# (W32# w32#) =
  let i# = word2Int# (word32ToWord# w32#)
      n# = intToInt8# i#
  in case int8ToInt# n# ==# i# of
    1# -> (# I8# n# | #)
    _  -> (# | (# #) #)

-- | Unsigned->signed, source fits in Int#, roundtrip at Int#
toInt16# :: Word32 -> (# Int16 | (# #) #)
toInt16# (W32# w32#) =
  let i# = word2Int# (word32ToWord# w32#)
      n# = intToInt16# i#
  in case int16ToInt# n# ==# i# of
    1# -> (# I16# n# | #)
    _  -> (# | (# #) #)

-- | Unsigned->signed, roundtrip at Int#
toInt32# :: Word32 -> (# Int32 | (# #) #)
toInt32# (W32# w32#) =
  let i# = word2Int# (word32ToWord# w32#)
      n# = intToInt32# i#
  in case int32ToInt# n# ==# i# of
    1# -> (# I32# n# | #)
    _  -> (# | (# #) #)

-- | Word32 fits in non-negative Int on all platforms, check via sign bit
toInt# :: Word32 -> (# Int | (# #) #)
toInt# (W32# w32#) =
  let i# = word2Int# (word32ToWord# w32#)
  in case i# >=# 0# of
    1# -> (# I# i# | #)
    _  -> (# | (# #) #)

-- | Bounds-checked float conversion
toFloat# :: Word32 -> (# Overflows | Float #)
toFloat# (W32# w32#) = case leWord# (word32ToWord# w32#) 16777215## of
  1# -> (# | F# (int2Float# (word2Int# (word32ToWord# w32#))) #)
  _  -> (# Overflow | #)
#endif
