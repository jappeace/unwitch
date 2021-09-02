{-# LANGUAGE TemplateHaskell #-}

module Unwitch.TH
  ( someSplice
  )
where

import Language.Haskell.TH
import Control.Monad

someSplice :: Q [Dec] -> Q [Dec]
someSplice x = join <$> replicateM 5 x
