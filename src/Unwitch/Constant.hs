module Unwitch.Constant
  ( maxIntegralRepDouble
  , maxIntegralRepFloat
  )
where

-- | The maximum integral value that can be unambiguously represented as a
-- 'Double'. Equal to 9,007,199,254,740,991 (2^53 - 1).
maxIntegralRepDouble :: Num a => a
maxIntegralRepDouble = 9007199254740991

-- | The maximum integral value that can be unambiguously represented as a
-- 'Float'. Equal to 16,777,215 (2^24 - 1).
maxIntegralRepFloat :: Num a => a
maxIntegralRepFloat = 16777215
