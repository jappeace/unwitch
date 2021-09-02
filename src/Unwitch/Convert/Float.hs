module Unwitch.Convert.Float
  ( toDouble
  )
where

import qualified GHC.Float as F
import           Prelude hiding (toRational, toInteger)

-- loses precision?!
toDouble :: Float -> Double
toDouble = F.float2Double

