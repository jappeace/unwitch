-- | Conversions from 'Float'.
module Unwitch.Convert.Float
  ( toDouble
  , toRational
  , toInteger
  , toInt8
  , toInt16
  , toInt32
  , toInt64
  , toInt
  , toWord8
  , toWord16
  , toWord32
  , toWord64
  , toWord
  , toNatural
  , ViaIntegerErrors(..)
  , IntegerErrors(..)
  , RationalErrors(..)
  )
where

import           Data.Bifunctor(first)
import           Unwitch.Constant
import qualified GHC.Float as F
import           Unwitch.Convert.Ratio(unwrapIfDenominatorOne)
import qualified Prelude
import           Unwitch.Errors
import           Prelude hiding (toRational, toInteger)
import qualified Unwitch.Convert.Integer as Integer
import Data.Word
import Data.Int
import Numeric.Natural (Natural)

-- | Lossless widening conversion.
toDouble :: Float -> Double
toDouble = F.float2Double

data IntegerErrors = IntegerFlow Integer Overflows
                   | RationalConversion RationalErrors
                   | DenomNotOne Rational
  deriving (Show, Eq)

data ViaIntegerErrors = MkInteger IntegerErrors
                      | BitConversionFailed Integer
  deriving (Show, Eq)

-- | Converts via 'Integer', fails if not a whole number or out of range.
toInt8 :: Float -> Either ViaIntegerErrors Int8
toInt8 = toViaInteger Integer.toInt8


-- | Converts via 'Integer', fails if not a whole number or out of range.
toInt16 :: Float -> Either ViaIntegerErrors Int16
toInt16 = toViaInteger Integer.toInt16


-- | Converts via 'Integer', fails if not a whole number or out of range.
toInt32 :: Float -> Either ViaIntegerErrors Int32
toInt32 = toViaInteger Integer.toInt32


-- | Converts via 'Integer', fails if not a whole number or out of range.
toInt64 :: Float -> Either ViaIntegerErrors Int64
toInt64 = toViaInteger Integer.toInt64


-- | Converts via 'Integer', fails if not a whole number or out of range.
toInt :: Float -> Either ViaIntegerErrors Int
toInt = toViaInteger Integer.toInt


-- | Converts via 'Integer', fails if not a whole number or out of range.
toWord8 :: Float -> Either ViaIntegerErrors Word8
toWord8 = toViaInteger Integer.toWord8


-- | Converts via 'Integer', fails if not a whole number or out of range.
toWord16 :: Float -> Either ViaIntegerErrors Word16
toWord16 = toViaInteger Integer.toWord16

-- | Converts via 'Integer', fails if not a whole number or out of range.
toWord32 :: Float -> Either ViaIntegerErrors Word32
toWord32 = toViaInteger Integer.toWord32

-- | Converts via 'Integer', fails if not a whole number or out of range.
toWord64 :: Float -> Either ViaIntegerErrors Word64
toWord64 = toViaInteger Integer.toWord64

-- | Converts via 'Integer', fails if not a whole number or out of range.
toWord :: Float -> Either ViaIntegerErrors Word
toWord = toViaInteger Integer.toWord

-- | Converts via 'Integer', fails if not a whole number, out of range, or negative.
toNatural :: Float -> Either ViaIntegerErrors Natural
toNatural float = do
  integer <- first MkInteger $ toInteger float
  case Integer.toNatural integer of
    Left err -> Left $ MkInteger $ IntegerFlow integer err
    Right n -> Right n

-- | Convert via 'Integer' then narrow, combining errors.
toViaInteger :: (Integer -> Maybe a) -> Float -> Either ViaIntegerErrors a
toViaInteger fun x = do
  integer <- first MkInteger $ toInteger x
  maybe (Left $ BitConversionFailed integer) Right $ fun integer

-- | Converts to 'Integer', fails if NaN, infinite, or has a fractional part.
toInteger :: Float -> Either IntegerErrors Integer
toInteger float = do
  rational <- first RationalConversion $ toRational float
  integer <- maybe (Left $ DenomNotOne rational) Right $ unwrapIfDenominatorOne rational
  if
    | integer < -maxIntegralRepFloat -> Left $ IntegerFlow integer Underflow
    | integer > maxIntegralRepFloat -> Left $ IntegerFlow integer Overflow
    | otherwise -> Right integer

data RationalErrors = IsNan
                    | IsInf Overflows
  deriving (Show, Eq)

toRational :: Float -> Either RationalErrors Rational
toRational float = if
  | isNaN float      -> Left IsNan
  | isInfinite float -> if
      | float > 0 -> Left $ IsInf Overflow
      | otherwise  -> Left $ IsInf Underflow
  | True              -> Right $ Prelude.toRational float
