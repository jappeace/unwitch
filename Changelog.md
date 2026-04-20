# Change log for unwitch project

## Version 3.0.0

### Breaking changes (GHC only)
+ Int32.toInt is now total on GHC (Int32 -> Int instead of Int32 -> Maybe Int).
  GHC's Int# is always at least 32 bits wide (WORD_SIZE_IN_BITS), so every
  Int32 value fits without loss. Code pattern matching on Just/Nothing will
  need updating.
+ On non-GHC compilers, Int32.toInt retains the Maybe Int return type since
  the Haskell Report only guarantees Int has 30 bits.
+ CInt.toInt now delegates to Int32.toInt instead of using fromIntegral.

### Added
+ Add MicroHs CI support — all exposed modules are compiled with mhs
+ Guard GHC-specific code behind CPP: unboxed types, GHC.Exts imports,
  MagicHash/UnboxedSums/UnboxedTuples extensions, and ghc-bignum dependency
+ Guard CInt module and all toCInt functions behind __GLASGOW_HASKELL__
  (MicroHs defines CInt as a newtype over Int, not Int32)
+ Replace GHC.Float.double2Float/float2Double with portable realToFrac
+ Replace Data.Text.all with Prelude.all for MicroHs compatibility
+ CI module list is now extracted from the cabal file automatically

## Version 2.2.0
+ New module Unwitch.Convert.CInt — conversions from CInt (Foreign.C.Types)
  to all supported numeric types (Int, Int8–64, Word, Word8–64, Integer,
  Natural, Float, Double)
+ Add toCInt to all 14 existing Convert modules (Int, Int8, Int16, Int32,
  Int64, Integer, Word, Word8, Word16, Word32, Word64, Natural, Float,
  Double)
+ Add CIntSpec test module with 12 tests covering total and fallible
  CInt conversions

## Version 2.1.0
+ Add Haddock documentation to partial conversion functions across
  all Convert modules — docs focus on non-obvious information only:
  float/double precision limits, via-Integer failure modes (NaN,
  infinity, fractional values), Underflow for negative-to-Natural,
  and Latin-1 encoding constraints
+ Remove redundant docs that merely restated the type signature

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

