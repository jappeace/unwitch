{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}

module Unwitch.Convert.Float
  ( toDouble
  )
where

import qualified GHC.Float as F
import           Prelude hiding (toRational, toInteger)
import Unwitch.TH

someSplice [d|y = 0|]


-- loses precision?!
toDouble :: Float -> Double
toDouble = F.float2Double

