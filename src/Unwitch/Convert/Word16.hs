-- | Conversions from 'Word16'.
module Unwitch.Convert.Word16
  ( -- * Conversions
    toWord8
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
  , toWord8#
  , toInt8#
  , toInt16#
  )
where

import qualified Data.Bits as Bits
import           Data.Word
import           Data.Int
import           Numeric.Natural (Natural)
import           Prelude hiding (toInteger)
import           GHC.Exts (word16ToWord#, word2Int#,
                           wordToWord8#, word8ToWord#,
                           intToInt8#, int8ToInt#,
                           intToInt16#, int16ToInt#,
                           eqWord#, (==#))
import           GHC.Int (Int8(..), Int16(..))
import           GHC.Word (Word8(..), Word16(..))

-- $unboxed
-- These use GHC unboxed types and unboxed sums for zero-allocation
-- failure handling. Requires the @MagicHash@, @UnboxedSums@ and
-- @UnboxedTuples@ language extensions.
-- See the <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/primitives.html GHC manual on unboxed types>.

-- | Narrowing conversion, returns 'Nothing' if out of range.
toWord8 :: Word16 -> Maybe Word8
toWord8 = Bits.toIntegralSized

-- | Lossless widening conversion.
toWord32 :: Word16 -> Word32
toWord32 = fromIntegral

-- | Lossless widening conversion.
toWord64 :: Word16 -> Word64
toWord64 = fromIntegral

-- | Lossless widening conversion.
toWord :: Word16 -> Word
toWord = fromIntegral

-- | Lossless conversion to 'Natural'.
toNatural :: Word16 -> Natural
toNatural = fromIntegral

-- | Unsigned-to-signed conversion, returns 'Nothing' if out of range.
toInt8 :: Word16 -> Maybe Int8
toInt8 = Bits.toIntegralSized

-- | Unsigned-to-signed conversion, returns 'Nothing' if out of range.
toInt16 :: Word16 -> Maybe Int16
toInt16 = Bits.toIntegralSized

-- | Lossless unsigned-to-signed conversion.
toInt32 :: Word16 -> Int32
toInt32 = fromIntegral

-- | Lossless unsigned-to-signed conversion.
toInt64 :: Word16 -> Int64
toInt64 = fromIntegral

-- | Lossless unsigned-to-signed conversion.
toInt :: Word16 -> Int
toInt = fromIntegral

-- | Lossless conversion to 'Integer'.
toInteger :: Word16 -> Integer
toInteger = fromIntegral

-- | Lossless conversion to 'Float'.
toFloat :: Word16 -> Float
toFloat = fromIntegral

-- | Lossless conversion to 'Double'.
toDouble :: Word16 -> Double
toDouble = fromIntegral

-- | Unsigned narrowing, roundtrip at Word#
toWord8# :: Word16 -> (# Word8 | (# #) #)
toWord8# (W16# w16#) =
  let w# = word16ToWord# w16#
      n# = wordToWord8# w#
  in case word8ToWord# n# `eqWord#` w# of
    1# -> (# W8# n# | #)
    _  -> (# | (# #) #)

-- | Unsigned->signed, source fits in Int#, roundtrip at Int#
toInt8# :: Word16 -> (# Int8 | (# #) #)
toInt8# (W16# w16#) =
  let i# = word2Int# (word16ToWord# w16#)
      n# = intToInt8# i#
  in case int8ToInt# n# ==# i# of
    1# -> (# I8# n# | #)
    _  -> (# | (# #) #)

-- | Unsigned->signed, source fits in Int#, roundtrip at Int#
toInt16# :: Word16 -> (# Int16 | (# #) #)
toInt16# (W16# w16#) =
  let i# = word2Int# (word16ToWord# w16#)
      n# = intToInt16# i#
  in case int16ToInt# n# ==# i# of
    1# -> (# I16# n# | #)
    _  -> (# | (# #) #)
