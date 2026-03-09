-- | Conversions from 'Double'.
module Unwitch.Convert.Double
  ( toFloat
  , toFixed
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
import           Data.Fixed (Fixed, HasResolution)
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

-- loses precision?!
toFloat :: Double -> Float
toFloat = F.double2Float

-- | Converts a Double to a Fixed value. Rejects NaN and infinities.
toFixed :: (HasResolution a) => Double -> Either RationalErrors (Fixed a)
toFixed double = do
  r <- toRational double
  Right $ Prelude.fromRational r

data IntegerErrors = IntegerFlow Integer Overflows
                   | RationalConversion RationalErrors
                   | DenomNotOne Rational
  deriving (Show, Eq)

data ViaIntegerErrors = MkInteger IntegerErrors
                      | BitConversionFailed Integer
  deriving (Show, Eq)

toInt8 :: Double -> Either ViaIntegerErrors Int8
toInt8 = toViaInteger Integer.toInt8

toInt16 :: Double -> Either ViaIntegerErrors Int16
toInt16 = toViaInteger Integer.toInt16

toInt32 :: Double -> Either ViaIntegerErrors Int32
toInt32 = toViaInteger Integer.toInt32

toInt64 :: Double -> Either ViaIntegerErrors Int64
toInt64 = toViaInteger Integer.toInt64

toInt :: Double -> Either ViaIntegerErrors Int
toInt = toViaInteger Integer.toInt

toWord8 :: Double -> Either ViaIntegerErrors Word8
toWord8 = toViaInteger Integer.toWord8

toWord16 :: Double -> Either ViaIntegerErrors Word16
toWord16 = toViaInteger Integer.toWord16

toWord32 :: Double -> Either ViaIntegerErrors Word32
toWord32 = toViaInteger Integer.toWord32

toWord64 :: Double -> Either ViaIntegerErrors Word64
toWord64 = toViaInteger Integer.toWord64

toWord :: Double -> Either ViaIntegerErrors Word
toWord = toViaInteger Integer.toWord

toNatural :: Double -> Either ViaIntegerErrors Natural
toNatural double = do
  integer <- first MkInteger $ toInteger double
  case Integer.toNatural integer of
    Left err -> Left $ MkInteger $ IntegerFlow integer err
    Right n -> Right n

toViaInteger :: (Integer -> Maybe a) -> Double -> Either ViaIntegerErrors a
toViaInteger fun x = do
  integer <- first MkInteger $ toInteger x
  maybe (Left $ BitConversionFailed integer) Right $ fun integer

toInteger :: Double -> Either IntegerErrors Integer
toInteger double = do
  rational <- first RationalConversion $ toRational double
  integer <- maybe (Left $ DenomNotOne rational) Right $ unwrapIfDenominatorOne rational
  if
    | integer < -maxIntegralRepDouble -> Left $ IntegerFlow integer Underflow
    | integer > maxIntegralRepDouble -> Left $ IntegerFlow integer Overflow
    | otherwise -> Right integer


data RationalErrors = IsNan
                    | IsInf Overflows
  deriving (Show, Eq)

toRational :: Double -> Either RationalErrors Rational
toRational double = if
  | isNaN double      -> Left IsNan
  | isInfinite double -> if
      | double > 0 -> Left $ IsInf Overflow
      | otherwise  -> Left $ IsInf Underflow
  | True              -> Right $ Prelude.toRational double

