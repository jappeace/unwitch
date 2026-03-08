module Unwitch.Convert.Complex
  ( fromReal
  , toReal
  , toReal#
  )
where

import Data.Complex (Complex((:+)), imagPart, realPart)

-- | Wraps a real number as a complex number with zero imaginary part.
fromReal :: (Num a) => a -> Complex a
fromReal x = x :+ 0

-- | Extracts the real part if the imaginary part is zero.
toReal :: (Eq a, Num a) => Complex a -> Maybe a
toReal c = if imagPart c == 0
  then Just $ realPart c
  else Nothing

-- | Unboxed variant of 'toReal'.
toReal# :: (Eq a, Num a) => Complex a -> (# a | (# #) #)
toReal# x = case toReal x of
  Just y  -> (# y | #)
  Nothing -> (# | (# #) #)
