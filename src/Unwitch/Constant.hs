module Unwitch.Constant
  ( maxIntegralRepDouble
  )
where

-- | The maximum integral value that can be unambiguously represented as a
-- 'Double'. Equal to 9,007,199,254,740,991.
maxIntegralRepDouble :: Num a => a
maxIntegralRepDouble = 9007199254740991
