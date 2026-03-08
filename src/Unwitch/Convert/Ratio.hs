module Unwitch.Convert.Ratio
  ( unwrapIfDenominatorOne
  , unwrapIfDenominatorOne#
  , fromIntegralToRatio
  , toFloat
  , toDouble
  )
where

import Data.Ratio(Ratio, (%))
import qualified Data.Ratio as Ratio

-- | Converts if denominator == 1
unwrapIfDenominatorOne :: (Eq a, Num a) => Ratio a -> Maybe a
unwrapIfDenominatorOne s = if Ratio.denominator s == 1 then
  Just $ Ratio.numerator s
  else Nothing

unwrapIfDenominatorOne# :: (Eq a, Num a) => Ratio a -> (# a | (# #) #)
unwrapIfDenominatorOne# x = case unwrapIfDenominatorOne x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

-- | Wraps an integral value as a Ratio with denominator 1.
fromIntegralToRatio :: (Integral a) => a -> Ratio a
fromIntegralToRatio x = x % 1

-- | Converts a Rational to Float. This is lossy for rationals
-- that cannot be exactly represented as Float.
toFloat :: Rational -> Float
toFloat = fromRational

-- | Converts a Rational to Double. This is lossy for rationals
-- that cannot be exactly represented as Double.
toDouble :: Rational -> Double
toDouble = fromRational
