-- | Error types for fallible numeric conversions.
module Unwitch.Errors
  ( Overflows(..)
  )
where

-- | Indicates that a numeric conversion failed because the source
-- value is outside the representable range of the target type.
data Overflows
  = Overflow   -- ^ The source value is above the target's upper bound.
  | Underflow  -- ^ The source value is below the target's lower bound.
  deriving (Show, Eq)
