module Unwitch.Convert.Float
  ( toDouble
  , toRational
  , toRational#
  , toInteger
  , toInteger#
  , toInt8
  , toInt8#
  , toInt16
  , toInt16#
  , toInt32
  , toInt32#
  , toInt64
  , toInt64#
  , toInt
  , toInt#
  , toWord8
  , toWord8#
  , toWord16
  , toWord16#
  , toWord32
  , toWord32#
  , toWord64
  , toWord64#
  , toWord
  , toWord#
  , toNatural
  , toNatural#
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

toDouble :: Float -> Double
toDouble = F.float2Double

data IntegerErrors = IntegerFlow Integer Overflows
                   | RationalConversion RationalErrors
                   | DenomNotOne Rational
  deriving (Show, Eq)

data ViaIntegerErrors = MkInteger IntegerErrors
                      | BitConversionFailed Integer
  deriving (Show, Eq)

toInt8 :: Float -> Either ViaIntegerErrors Int8
toInt8 = toViaInteger Integer.toInt8

-- | Unboxed variant of 'toInt8'.
toInt8# :: Float -> (# ViaIntegerErrors | Int8 #)
toInt8# x = case toInt8 x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toInt16 :: Float -> Either ViaIntegerErrors Int16
toInt16 = toViaInteger Integer.toInt16

-- | Unboxed variant of 'toInt16'.
toInt16# :: Float -> (# ViaIntegerErrors | Int16 #)
toInt16# x = case toInt16 x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toInt32 :: Float -> Either ViaIntegerErrors Int32
toInt32 = toViaInteger Integer.toInt32

-- | Unboxed variant of 'toInt32'.
toInt32# :: Float -> (# ViaIntegerErrors | Int32 #)
toInt32# x = case toInt32 x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toInt64 :: Float -> Either ViaIntegerErrors Int64
toInt64 = toViaInteger Integer.toInt64

-- | Unboxed variant of 'toInt64'.
toInt64# :: Float -> (# ViaIntegerErrors | Int64 #)
toInt64# x = case toInt64 x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toInt :: Float -> Either ViaIntegerErrors Int
toInt = toViaInteger Integer.toInt

-- | Unboxed variant of 'toInt'.
toInt# :: Float -> (# ViaIntegerErrors | Int #)
toInt# x = case toInt x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toWord8 :: Float -> Either ViaIntegerErrors Word8
toWord8 = toViaInteger Integer.toWord8

-- | Unboxed variant of 'toWord8'.
toWord8# :: Float -> (# ViaIntegerErrors | Word8 #)
toWord8# x = case toWord8 x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toWord16 :: Float -> Either ViaIntegerErrors Word16
toWord16 = toViaInteger Integer.toWord16

-- | Unboxed variant of 'toWord16'.
toWord16# :: Float -> (# ViaIntegerErrors | Word16 #)
toWord16# x = case toWord16 x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toWord32 :: Float -> Either ViaIntegerErrors Word32
toWord32 = toViaInteger Integer.toWord32

-- | Unboxed variant of 'toWord32'.
toWord32# :: Float -> (# ViaIntegerErrors | Word32 #)
toWord32# x = case toWord32 x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toWord64 :: Float -> Either ViaIntegerErrors Word64
toWord64 = toViaInteger Integer.toWord64

-- | Unboxed variant of 'toWord64'.
toWord64# :: Float -> (# ViaIntegerErrors | Word64 #)
toWord64# x = case toWord64 x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toWord :: Float -> Either ViaIntegerErrors Word
toWord = toViaInteger Integer.toWord

-- | Unboxed variant of 'toWord'.
toWord# :: Float -> (# ViaIntegerErrors | Word #)
toWord# x = case toWord x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toNatural :: Float -> Either ViaIntegerErrors Natural
toNatural float = do
  integer <- first MkInteger $ toInteger float
  case Integer.toNatural integer of
    Left err -> Left $ MkInteger $ IntegerFlow integer err
    Right n -> Right n

-- | Unboxed variant of 'toNatural'.
toNatural# :: Float -> (# ViaIntegerErrors | Natural #)
toNatural# x = case toNatural x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toViaInteger :: (Integer -> Maybe a) -> Float -> Either ViaIntegerErrors a
toViaInteger fun x = do
  integer <- first MkInteger $ toInteger x
  maybe (Left $ BitConversionFailed integer) Right $ fun integer

toInteger :: Float -> Either IntegerErrors Integer
toInteger float = do
  rational <- first RationalConversion $ toRational float
  integer <- maybe (Left $ DenomNotOne rational) Right $ unwrapIfDenominatorOne rational
  if
    | integer < -maxIntegralRepFloat -> Left $ IntegerFlow integer Underflow
    | integer > maxIntegralRepFloat -> Left $ IntegerFlow integer Overflow
    | otherwise -> Right integer

-- | Unboxed variant of 'toInteger'.
toInteger# :: Float -> (# IntegerErrors | Integer #)
toInteger# x = case toInteger x of
  Left e  -> (# e | #)
  Right y -> (# | y #)


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

-- | Unboxed variant of 'toRational'.
toRational# :: Float -> (# RationalErrors | Rational #)
toRational# x = case toRational x of
  Left e  -> (# e | #)
  Right y -> (# | y #)
