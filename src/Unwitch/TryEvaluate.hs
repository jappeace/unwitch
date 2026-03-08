module Unwitch.TryEvaluate
  ( tryEvaluate
  )
where

import Control.Exception (Exception, evaluate, try)
import System.IO.Unsafe (unsafePerformIO)

tryEvaluate :: Exception e => a -> Either e a
tryEvaluate = unsafePerformIO . try . evaluate
{-# NOINLINE tryEvaluate #-}
