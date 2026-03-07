module Main where

import Test.Hspec
import qualified Test.TestSpec
import qualified Test.Convert.Int8Spec
import qualified Test.Convert.Int16Spec
import qualified Test.Convert.Int32Spec
import qualified Test.Convert.Int64Spec
import qualified Test.Convert.IntSpec
import qualified Test.Convert.Word8Spec
import qualified Test.Convert.Word16Spec
import qualified Test.Convert.Word32Spec
import qualified Test.Convert.Word64Spec
import qualified Test.Convert.WordSpec
import qualified Test.Convert.NaturalSpec
import qualified Test.Convert.IntegerSpec
import qualified Test.Convert.FloatSpec
import qualified Test.Convert.DoubleSpec

main :: IO ()
main = hspec $ do
  Test.TestSpec.spec
  Test.Convert.Int8Spec.spec
  Test.Convert.Int16Spec.spec
  Test.Convert.Int32Spec.spec
  Test.Convert.Int64Spec.spec
  Test.Convert.IntSpec.spec
  Test.Convert.Word8Spec.spec
  Test.Convert.Word16Spec.spec
  Test.Convert.Word32Spec.spec
  Test.Convert.Word64Spec.spec
  Test.Convert.WordSpec.spec
  Test.Convert.NaturalSpec.spec
  Test.Convert.IntegerSpec.spec
  Test.Convert.FloatSpec.spec
  Test.Convert.DoubleSpec.spec
