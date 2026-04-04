-- | Conversions from 'Int8'.
module Unwitch.Convert.Int8
  ( -- * Conversions
    toInt16
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
  , toCInt
#ifdef __GLASGOW_HASKELL__
  -- * Unboxed conversions
  -- $unboxed
  , toWord8#
  , toWord16#
  , toWord32#
  , toWord64#
  , toWord#
  , toNatural#
#endif
  )
where

import           Unwitch.Errors
import qualified Data.Bits as Bits
import           Data.Word
import           Data.Int
import           Numeric.Natural (Natural)
import           Foreign.C.Types (CInt(CInt))
import           Prelude hiding (toInteger)
#ifdef __GLASGOW_HASKELL__
import           GHC.Exts (Word(..), int8ToInt#, int2Word#,
                           wordToWord8#, wordToWord16#, wordToWord32#,
                           wordToWord64#, (>=#))
import           GHC.Int (Int8(..))
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

toInt16 :: Int8 -> Int16
toInt16 = fromIntegral

toInt32 :: Int8 -> Int32
toInt32 = fromIntegral

toInt64 :: Int8 -> Int64
toInt64 = fromIntegral

toInt :: Int8 -> Int
toInt = fromIntegral

toInteger :: Int8 -> Integer
toInteger = fromIntegral

toWord8 :: Int8 -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Int8 -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Int8 -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord64 :: Int8 -> Maybe Word64
toWord64 = Bits.toIntegralSized

toWord :: Int8 -> Maybe Word
toWord = Bits.toIntegralSized

-- | Signed-to-unsigned conversion, returns 'Left' 'Underflow' for negative values.
toNatural :: Int8 -> Either Overflows Natural
toNatural x = if
  | x < 0     -> Left Underflow
  | otherwise  -> Right $ fromIntegral x

-- | Widening conversion via Int32, always succeeds.
toCInt :: Int8 -> CInt
toCInt x = CInt $ toInt32 x

toFloat :: Int8 -> Float
toFloat = fromIntegral

toDouble :: Int8 -> Double
toDouble = fromIntegral

#ifdef __GLASGOW_HASKELL__
-- | Signed->unsigned, check non-negative
toWord8# :: Int8 -> (# Word8 | (# #) #)
toWord8# (I8# x8#) = case int8ToInt# x8# >=# 0# of
  1# -> (# W8# (wordToWord8# (int2Word# (int8ToInt# x8#))) | #)
  _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative
toWord16# :: Int8 -> (# Word16 | (# #) #)
toWord16# (I8# x8#) = case int8ToInt# x8# >=# 0# of
  1# -> (# W16# (wordToWord16# (int2Word# (int8ToInt# x8#))) | #)
  _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative
toWord32# :: Int8 -> (# Word32 | (# #) #)
toWord32# (I8# x8#) = case int8ToInt# x8# >=# 0# of
  1# -> (# W32# (wordToWord32# (int2Word# (int8ToInt# x8#))) | #)
  _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative
toWord64# :: Int8 -> (# Word64 | (# #) #)
toWord64# (I8# x8#) = case int8ToInt# x8# >=# 0# of
  1# -> (# W64# (wordToWord64# (int2Word# (int8ToInt# x8#))) | #)
  _  -> (# | (# #) #)

-- | Signed->unsigned, check non-negative
toWord# :: Int8 -> (# Word | (# #) #)
toWord# (I8# x8#) = case int8ToInt# x8# >=# 0# of
  1# -> (# W# (int2Word# (int8ToInt# x8#)) | #)
  _  -> (# | (# #) #)

-- | Check non-negative, construct NS directly
toNatural# :: Int8 -> (# Overflows | Natural #)
toNatural# (I8# x8#) = case int8ToInt# x8# >=# 0# of
  1# -> (# | NS (int2Word# (int8ToInt# x8#)) #)
  _  -> (# Underflow | #)
#endif
