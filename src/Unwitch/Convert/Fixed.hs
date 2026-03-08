module Unwitch.Convert.Fixed
  ( fromInteger
  , toInteger
  , toInteger#
  , toRational
  , toFixed
  , toFixed#
  )
where

import Data.Fixed (Fixed, HasResolution)
import Data.Ratio (denominator, numerator)
import qualified Prelude
import Prelude hiding (fromInteger, toInteger, toRational)

-- | Converts an Integer to a Fixed value. Infallible.
fromInteger :: (HasResolution a) => Integer -> Fixed a
fromInteger = Prelude.fromInteger

-- | Converts a Fixed value to Integer, succeeding only if there is
-- no fractional part (i.e. the value is a whole number).
toInteger :: (HasResolution a) => Fixed a -> Maybe Integer
toInteger fixed =
  let r = Prelude.toRational fixed
  in if denominator r == 1
     then Just $ numerator r
     else Nothing

-- | Unboxed variant of 'toInteger'.
toInteger# :: (HasResolution a) => Fixed a -> (# Integer | (# #) #)
toInteger# x = case toInteger x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)

-- | Converts a Fixed value to Rational. Infallible, exact.
toRational :: (HasResolution a) => Fixed a -> Rational
toRational = Prelude.toRational

-- | Converts between Fixed types with potentially different resolutions.
-- Succeeds only if the value can be exactly represented in the target resolution.
toFixed :: (HasResolution a, HasResolution b) => Fixed a -> Maybe (Fixed b)
toFixed source =
  let r = Prelude.toRational source
      target = Prelude.fromRational r
  in if Prelude.toRational target == r
     then Just target
     else Nothing

-- | Unboxed variant of 'toFixed'.
toFixed# :: (HasResolution a, HasResolution b) => Fixed a -> (# Fixed b | (# #) #)
toFixed# x = case toFixed x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)
