-- | Conversions from 'Int64'.
module Unwitch.Convert.Int64
  ( -- * Conversions
    toInt8
  , toInt16
  , toInt32
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
#ifdef __GLASGOW_HASKELL__
  -- * Unboxed conversions
  -- $unboxed
  , toInt8#
  , toInt16#
  , toInt32#
  , toInt#
  , toWord8#
  , toWord16#
  , toWord32#
  , toWord64#
  , toWord#
  , toNatural#
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
import           Foreign.C.Types (CInt(CInt))
import           Prelude hiding (toInteger)
#ifdef __GLASGOW_HASKELL__
import           GHC.Exts (Int(..), Word(..), Float(..), Double(..),
                           int64ToInt#, intToInt64#,
                           intToInt8#, int8ToInt#,
                           intToInt16#, int16ToInt#,
                           intToInt32#, int32ToInt#,
                           int2Word#, word2Int#,
                           wordToWord8#, word8ToWord#,
                           wordToWord16#, word16ToWord#,
                           wordToWord32#, word32ToWord#,
                           wordToWord64#,
                           int2Float#, int2Double#,
                           (<#), (>#),
                           eqInt64#, geInt64#)
import           GHC.Int (Int8(..), Int16(..), Int32(..), Int64(..))
import           GHC.Word (Word8(..), Word16(..), Word32(..), Word64(..))
import           GHC.Num.Natural (Natural(NS))
#endif

#ifdef __GLASGOW_HASKELL__
-- $unboxed
-- These use GHC unboxed types and unboxed sums for zero-allocation
-- failure handling. Requires the @MagicHash@, @UnboxedSums@ and
-- @UnboxedTuples@ language extensions.
-- See the <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/primitives.html GHC manual on unboxed types>.
#endif

toInt8 :: Int64 -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Int64 -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt32 :: Int64 -> Maybe Int32
toInt32 = Bits.toIntegralSized

toInt :: Int64 -> Maybe Int
toInt = Bits.toIntegralSized

-- | Narrowing conversion via Int32, fails if outside Int32 range.
toCInt :: Int64 -> Maybe CInt
toCInt x = CInt <$> toInt32 x

toInteger :: Int64 -> Integer
toInteger = fromIntegral

toWord8 :: Int64 -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Int64 -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Int64 -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord64 :: Int64 -> Maybe Word64
toWord64 = Bits.toIntegralSized

toWord :: Int64 -> Maybe Word
toWord = Bits.toIntegralSized

-- | Signed-to-unsigned conversion, returns 'Left' 'Underflow' for negative values.
toNatural :: Int64 -> Either Overflows Natural
toNatural x = if
  | x < 0     -> Left Underflow
  | otherwise  -> Right $ fromIntegral x

-- | Checked conversion, fails if outside exact float integer range (+-16777215).
toFloat :: Int64 -> Either Overflows Float
toFloat x = if
  | x < -maxIntegralRepFloat -> Left Underflow
  | x > maxIntegralRepFloat  -> Left Overflow
  | otherwise                -> Right $ fromIntegral x

-- | Checked conversion, fails if outside exact double integer range (+-9007199254740991).
toDouble :: Int64 -> Either Overflows Double
toDouble x = if
  | x < -maxIntegralRepDouble -> Left Underflow
  | x > maxIntegralRepDouble  -> Left Overflow
  | otherwise                 -> Right $ fromIntegral x

#ifdef __GLASGOW_HASKELL__
-- | Narrow through Int#, compare at Int64#
toInt8# :: Int64 -> (# Int8 | (# #) #)
toInt8# (I64# x64#) =
  let i# = int64ToInt# x64#
      n# = intToInt8# i#
  in case eqInt64# (intToInt64# (int8ToInt# n#)) x64# of
    1# -> (# I8# n# | #)
    _  -> (# | (# #) #)

-- | Signed narrowing via Int64# comparison
toInt16# :: Int64 -> (# Int16 | (# #) #)
toInt16# (I64# x64#) =
  let i# = int64ToInt# x64#
      n# = intToInt16# i#
  in case eqInt64# (intToInt64# (int16ToInt# n#)) x64# of
    1# -> (# I16# n# | #)
    _  -> (# | (# #) #)

-- | Signed narrowing via Int64# comparison
toInt32# :: Int64 -> (# Int32 | (# #) #)
toInt32# (I64# x64#) =
  let i# = int64ToInt# x64#
      n# = intToInt32# i#
  in case eqInt64# (intToInt64# (int32ToInt# n#)) x64# of
    1# -> (# I32# n# | #)
    _  -> (# | (# #) #)

-- | Roundtrip check at Int64# level for platform safety
toInt# :: Int64 -> (# Int | (# #) #)
toInt# (I64# x64#) =
  let i# = int64ToInt# x64#
  in case eqInt64# (intToInt64# i#) x64# of
    1# -> (# I# i# | #)
    _  -> (# | (# #) #)

-- | Signed->unsigned narrow via Int64# comparison
toWord8# :: Int64 -> (# Word8 | (# #) #)
toWord8# (I64# x64#) =
  let i# = int64ToInt# x64#
      n# = wordToWord8# (int2Word# i#)
  in case eqInt64# (intToInt64# (word2Int# (word8ToWord# n#))) x64# of
    1# -> (# W8# n# | #)
    _  -> (# | (# #) #)

-- | Signed->unsigned narrow via Int64# comparison
toWord16# :: Int64 -> (# Word16 | (# #) #)
toWord16# (I64# x64#) =
  let i# = int64ToInt# x64#
      n# = wordToWord16# (int2Word# i#)
  in case eqInt64# (intToInt64# (word2Int# (word16ToWord# n#))) x64# of
    1# -> (# W16# n# | #)
    _  -> (# | (# #) #)

-- | Signed->unsigned narrow via Int64# comparison
toWord32# :: Int64 -> (# Word32 | (# #) #)
toWord32# (I64# x64#) =
  let i# = int64ToInt# x64#
      n# = wordToWord32# (int2Word# i#)
  in case eqInt64# (intToInt64# (word2Int# (word32ToWord# n#))) x64# of
    1# -> (# W32# n# | #)
    _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative at Int64# level
toWord64# :: Int64 -> (# Word64 | (# #) #)
toWord64# (I64# x64#) = case geInt64# x64# (intToInt64# 0#) of
  1# -> (# W64# (wordToWord64# (int2Word# (int64ToInt# x64#))) | #)
  _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative at Int64# then roundtrip
toWord# :: Int64 -> (# Word | (# #) #)
toWord# (I64# x64#) = case geInt64# x64# (intToInt64# 0#) of
  1# -> let i# = int64ToInt# x64#
         in case eqInt64# (intToInt64# i#) x64# of
           1# -> (# W# (int2Word# i#) | #)
           _  -> (# | (# #) #)
  _  -> (# | (# #) #)

-- | Check non-negative at Int64# level, construct NS
toNatural# :: Int64 -> (# Overflows | Natural #)
toNatural# (I64# x64#) = case geInt64# x64# (intToInt64# 0#) of
  1# -> let i# = int64ToInt# x64#
         in case eqInt64# (intToInt64# i#) x64# of
           1# -> (# | NS (int2Word# i#) #)
           _  -> (# Overflow | #)
  _  -> (# Underflow | #)

-- | Bounds-checked float conversion via Int#
toFloat# :: Int64 -> (# Overflows | Float #)
toFloat# (I64# x64#) =
  let i# = int64ToInt# x64#
  in case eqInt64# (intToInt64# i#) x64# of
    1# -> case i# <# -16777215# of
      1# -> (# Underflow | #)
      _  -> case i# ># 16777215# of
        1# -> (# Overflow | #)
        _  -> (# | F# (int2Float# i#) #)
    _  -> case geInt64# x64# (intToInt64# 0#) of
      1# -> (# Overflow | #)
      _  -> (# Underflow | #)

-- | Bounds-checked double conversion via Int#
toDouble# :: Int64 -> (# Overflows | Double #)
toDouble# (I64# x64#) =
  let i# = int64ToInt# x64#
  in case eqInt64# (intToInt64# i#) x64# of
    1# -> case i# <# -9007199254740991# of
      1# -> (# Underflow | #)
      _  -> case i# ># 9007199254740991# of
        1# -> (# Overflow | #)
        _  -> (# | D# (int2Double# i#) #)
    _  -> case geInt64# x64# (intToInt64# 0#) of
      1# -> (# Overflow | #)
      _  -> (# Underflow | #)
#endif
