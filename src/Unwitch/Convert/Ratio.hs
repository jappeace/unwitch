module Unwitch.Convert.Ratio
  ( unwrapIfDenominatorOne
  )
where

import Data.Ratio(Ratio)
import qualified Data.Ratio as Ratio

-- | Converts if denominator == 1
unwrapIfDenominatorOne :: (Eq a, Num a) => Ratio a -> Maybe a
unwrapIfDenominatorOne s = if Ratio.denominator s == 1 then
  Just $ Ratio.numerator s
  else Nothing
