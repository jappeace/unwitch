module Unwitch.Errors
  ( Overflows(..)
  )
where

data Overflows = Overflow
               | Underflow
  deriving (Show, Eq)
