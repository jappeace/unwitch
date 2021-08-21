module Unwitch.Convert.Double
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
toFloat :: Double -> Float
toFloat = F.double2Float

data IntegerErrors = IntegerFlow Integer Overflows
                   | RationalConversion RationalErrors
                   | DenomNotOne Rational

toInteger :: Double -> Either IntegerErrors Integer
toInteger double = do
  rational <- first RationalConversion $ toRational double
  integer <- maybe (Left $ DenomNotOne rational) Right $ unwrapIfDenominatorOne rational
  if
    | integer < -maxIntegralRepDouble -> Left $ IntegerFlow integer Underflow
    | integer > maxIntegralRepDouble -> Left $ IntegerFlow integer Overflow
    | otherwise -> Right integer


data RationalErrors = IsNan
                    | IsInf Overflows

toRational :: Double -> Either RationalErrors Rational
toRational double = if
  | isNaN double      -> Left IsNan
  | isInfinite double -> if
      | double > 0 -> Left $ IsInf Overflow
      | otherwise  -> Left $ IsInf Underflow
  | True              -> Right $ Prelude.toRational double

