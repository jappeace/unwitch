module Unwitch.Convert.Integer
  ( toDouble
  )
where

import           Unwitch.Errors
import           Unwitch.Constant

toDouble :: Integer -> Either Overflows Double
toDouble integer = if
    | integer < -maxIntegralRepDouble -> Left Underflow
    | integer > maxIntegralRepDouble -> Left Overflow
    | otherwise -> Right $ Prelude.fromIntegral integer
