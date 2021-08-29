module Unwitch.Convert.Float
  ( toFloat
  , toRational
  , toInteger
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

-- loses precision?!
toDouble :: Float -> Double
toDouble = F.float2Double

