# Change log for unwitch project

## Version 2.0.2
+ Wire up 9 missing test specs that existed but were not included in the test runner
+ Add module-level haddock to all Convert modules, Errors, and Constant
+ Add haddock to Overflows type and constructors
+ Expand property tests with 20+ new properties covering Int/Word
  same-width conversions, Float/Double range checks, cross-sign
  narrowing, Ratio round-trips, and Fixed round-trips

## Version 2.0.1
+ Add haddock section headers separating boxed and unboxed conversions
+ Each module with unboxed functions now has "Conversions" and
  "Unboxed conversions" sections with documentation explaining
  zero-allocation failure handling and required GHC extensions.

## Version 2.0.0
+ fix cabal file links
+ delete complex module which made no sense.

## Version 1.0.0 
+ claude generate most conversion functions, looks good

## Version 0.1.0 

Implement some of Double.
Write description

## Version 0.0.0 

import [template](https://github.com/jappeace/haskell-template-project).

