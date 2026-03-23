-- | Conversions from 'CInt'.
module Unwitch.Convert.CInt
  ( -- * Conversions
    toInt8
  , toInt16
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
  )
where

import           Unwitch.Errors
import qualified Unwitch.Convert.Int32 as Int32
import           Data.Word
import           Data.Int
import           Numeric.Natural (Natural)
import           Prelude hiding (toInteger)
import           Foreign.C.Types (CInt(CInt))

-- | Narrowing conversion, fails if outside Int8 range.
toInt8 :: CInt -> Maybe Int8
toInt8 (CInt x) = Int32.toInt8 x

-- | Narrowing conversion, fails if outside Int16 range.
toInt16 :: CInt -> Maybe Int16
toInt16 (CInt x) = Int32.toInt16 x

-- | Unwrap the underlying Int32.
toInt32 :: CInt -> Int32
toInt32 (CInt x) = x

-- | Widening conversion, always succeeds.
toInt64 :: CInt -> Int64
toInt64 (CInt x) = Int32.toInt64 x

-- | Total conversion, Int is at least 32 bits wide.
toInt :: CInt -> Int
toInt (CInt x) = fromIntegral x

-- | Total conversion to Integer.
toInteger :: CInt -> Integer
toInteger (CInt x) = Int32.toInteger x

-- | Signed-to-unsigned narrowing, fails if negative or out of range.
toWord8 :: CInt -> Maybe Word8
toWord8 (CInt x) = Int32.toWord8 x

-- | Signed-to-unsigned narrowing, fails if negative or out of range.
toWord16 :: CInt -> Maybe Word16
toWord16 (CInt x) = Int32.toWord16 x

-- | Signed-to-unsigned, fails if negative.
toWord32 :: CInt -> Maybe Word32
toWord32 (CInt x) = Int32.toWord32 x

-- | Signed-to-unsigned, fails if negative.
toWord64 :: CInt -> Maybe Word64
toWord64 (CInt x) = Int32.toWord64 x

-- | Signed-to-unsigned, fails if negative.
toWord :: CInt -> Maybe Word
toWord (CInt x) = Int32.toWord x

-- | Signed-to-unsigned, returns 'Left' 'Underflow' for negative values.
toNatural :: CInt -> Either Overflows Natural
toNatural (CInt x) = Int32.toNatural x

-- | Checked conversion, fails if outside exact float integer range (+-16777215).
toFloat :: CInt -> Either Overflows Float
toFloat (CInt x) = Int32.toFloat x

-- | Total conversion, all Int32 values are exactly representable as Double.
toDouble :: CInt -> Double
toDouble (CInt x) = Int32.toDouble x
