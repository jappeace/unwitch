-- | Conversions from 'Word8'.
module Unwitch.Convert.Word8
  ( -- * Conversions
    toWord16
  , toWord32
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
  -- * Unboxed conversions
  -- $unboxed
  , toInt8#
  )
where

import qualified Data.Bits as Bits
import           Data.Word
import           Data.Int
import           Numeric.Natural (Natural)
import           Prelude hiding (toInteger)
import           GHC.Exts (word8ToWord#, word2Int#, intToInt8#, int8ToInt#,
                           (==#))
import           GHC.Int (Int8(..))
import           GHC.Word (Word8(..))

-- $unboxed
-- These use GHC unboxed types and unboxed sums for zero-allocation
-- failure handling. Requires the @MagicHash@, @UnboxedSums@ and
-- @UnboxedTuples@ language extensions.
-- See the <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/primitives.html GHC manual on unboxed types>.

-- | Lossless widening conversion.
toWord16 :: Word8 -> Word16
toWord16 = fromIntegral

-- | Lossless widening conversion.
toWord32 :: Word8 -> Word32
toWord32 = fromIntegral

-- | Lossless widening conversion.
toWord64 :: Word8 -> Word64
toWord64 = fromIntegral

-- | Lossless widening conversion.
toWord :: Word8 -> Word
toWord = fromIntegral

-- | Lossless conversion to 'Natural'.
toNatural :: Word8 -> Natural
toNatural = fromIntegral

-- | Unsigned-to-signed conversion, returns 'Nothing' if out of range.
toInt8 :: Word8 -> Maybe Int8
toInt8 = Bits.toIntegralSized

-- | Lossless unsigned-to-signed conversion.
toInt16 :: Word8 -> Int16
toInt16 = fromIntegral

-- | Lossless unsigned-to-signed conversion.
toInt32 :: Word8 -> Int32
toInt32 = fromIntegral

-- | Lossless unsigned-to-signed conversion.
toInt64 :: Word8 -> Int64
toInt64 = fromIntegral

-- | Lossless unsigned-to-signed conversion.
toInt :: Word8 -> Int
toInt = fromIntegral

-- | Lossless conversion to 'Integer'.
toInteger :: Word8 -> Integer
toInteger = fromIntegral

-- | Lossless conversion to 'Float'.
toFloat :: Word8 -> Float
toFloat = fromIntegral

-- | Lossless conversion to 'Double'.
toDouble :: Word8 -> Double
toDouble = fromIntegral

-- | Unsigned->signed, source fits in Int#, roundtrip at Int#
toInt8# :: Word8 -> (# Int8 | (# #) #)
toInt8# (W8# w8#) =
  let i# = word2Int# (word8ToWord# w8#)
      n# = intToInt8# i#
  in case int8ToInt# n# ==# i# of
    1# -> (# I8# n# | #)
    _  -> (# | (# #) #)
