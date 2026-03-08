module Unwitch.Convert.Word8
  ( toWord16
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

toWord16 :: Word8 -> Word16
toWord16 = fromIntegral

toWord32 :: Word8 -> Word32
toWord32 = fromIntegral

toWord64 :: Word8 -> Word64
toWord64 = fromIntegral

toWord :: Word8 -> Word
toWord = fromIntegral

toNatural :: Word8 -> Natural
toNatural = fromIntegral

toInt8 :: Word8 -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Word8 -> Int16
toInt16 = fromIntegral

toInt32 :: Word8 -> Int32
toInt32 = fromIntegral

toInt64 :: Word8 -> Int64
toInt64 = fromIntegral

toInt :: Word8 -> Int
toInt = fromIntegral

toInteger :: Word8 -> Integer
toInteger = fromIntegral

toFloat :: Word8 -> Float
toFloat = fromIntegral

toDouble :: Word8 -> Double
toDouble = fromIntegral

-- | Pattern E: unsigned->signed, source fits in Int#, roundtrip at Int#
toInt8# :: Word8 -> (# Int8 | (# #) #)
toInt8# (W8# w8#) =
  let i# = word2Int# (word8ToWord# w8#)
      n# = intToInt8# i#
  in case int8ToInt# n# ==# i# of
    1# -> (# I8# n# | #)
    _  -> (# | (# #) #)
