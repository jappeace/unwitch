module Unwitch.Convert.Integer
  ( toDouble
  , toDouble#
  , toFloat
  , toFloat#
  , toNatural
  , toNatural#
  , toInt8
  , toInt8#
  , toInt16
  , toInt16#
  , toInt32
  , toInt32#
  , toInt64
  , toInt64#
  , toInt
  , toInt#
  , toWord8
  , toWord8#
  , toWord16
  , toWord16#
  , toWord32
  , toWord32#
  , toWord64
  , toWord64#
  , toWord
  , toWord#
  )
where

import           Unwitch.Errors
import           Unwitch.Constant
import qualified Data.Bits as Bits
import Data.Word
import Data.Int
import Numeric.Natural (Natural)
import           GHC.Exts (Int(..), Word(..), Float(..), Double(..),
                           intToInt8#, int8ToInt#,
                           intToInt16#, int16ToInt#,
                           intToInt32#, int32ToInt#,
                           intToInt64#,
                           int2Word#, word2Int#,
                           wordToWord8#, word8ToWord#,
                           wordToWord16#, word16ToWord#,
                           wordToWord32#, word32ToWord#,
                           wordToWord64#,
                           int2Float#, int2Double#,
                           (==#), (>=#), (<#), (>#))
import           GHC.Int (Int8(..), Int16(..), Int32(..), Int64(..))
import           GHC.Word (Word8(..), Word16(..), Word32(..), Word64(..))
import           GHC.Num.Integer (Integer(..), integerToWord#,
                                  integerFromWord#, integerEq#)
import           GHC.Num.Natural (Natural(NS, NB))


toDouble :: Integer -> Either Overflows Double
toDouble integer = if
    | integer < -maxIntegralRepDouble -> Left Underflow
    | integer > maxIntegralRepDouble -> Left Overflow
    | otherwise -> Right $ Prelude.fromIntegral integer

toFloat :: Integer -> Either Overflows Float
toFloat integer = if
    | integer < -maxIntegralRepFloat -> Left Underflow
    | integer > maxIntegralRepFloat -> Left Overflow
    | otherwise -> Right $ Prelude.fromIntegral integer

toNatural :: Integer -> Either Overflows Natural
toNatural integer = if
    | integer < 0 -> Left Underflow
    | otherwise -> Right $ Prelude.fromIntegral integer

toInt8 :: Integer -> Maybe Int8
toInt8 = Bits.toIntegralSized

toInt16 :: Integer -> Maybe Int16
toInt16 = Bits.toIntegralSized

toInt32 :: Integer -> Maybe Int32
toInt32 = Bits.toIntegralSized

toInt64 :: Integer -> Maybe Int64
toInt64 = Bits.toIntegralSized

toInt :: Integer -> Maybe Int
toInt = Bits.toIntegralSized

toWord8 :: Integer -> Maybe Word8
toWord8 = Bits.toIntegralSized

toWord16 :: Integer -> Maybe Word16
toWord16 = Bits.toIntegralSized

toWord32 :: Integer -> Maybe Word32
toWord32 = Bits.toIntegralSized

toWord64 :: Integer -> Maybe Word64
toWord64 = Bits.toIntegralSized

toWord :: Integer -> Maybe Word
toWord = Bits.toIntegralSized

-- | Bounds-checked double conversion via IS/IP/IN
toDouble# :: Integer -> (# Overflows | Double #)
toDouble# x = case x of
  IS i# -> case i# <# -9007199254740991# of
    1# -> (# Underflow | #)
    _  -> case i# ># 9007199254740991# of
      1# -> (# Overflow | #)
      _  -> (# | D# (int2Double# i#) #)
  IP _ -> (# Overflow | #)
  IN _ -> (# Underflow | #)

-- | Bounds-checked float conversion via IS/IP/IN
toFloat# :: Integer -> (# Overflows | Float #)
toFloat# x = case x of
  IS i# -> case i# <# -16777215# of
    1# -> (# Underflow | #)
    _  -> case i# ># 16777215# of
      1# -> (# Overflow | #)
      _  -> (# | F# (int2Float# i#) #)
  IP _ -> (# Overflow | #)
  IN _ -> (# Underflow | #)

-- | Integer->Natural via IS/IP/IN
toNatural# :: Integer -> (# Overflows | Natural #)
toNatural# x = case x of
  IS i# -> case i# >=# 0# of
    1# -> (# | NS (int2Word# i#) #)
    _  -> (# Underflow | #)
  IP ba# -> (# | NB ba# #)
  IN _ -> (# Underflow | #)

-- | Integer->Int8 via IS/IP/IN, narrow and roundtrip at Int#
toInt8# :: Integer -> (# Int8 | (# #) #)
toInt8# x = case x of
  IS i# ->
    let n# = intToInt8# i#
    in case int8ToInt# n# ==# i# of
      1# -> (# I8# n# | #)
      _  -> (# | (# #) #)
  IP _ -> (# | (# #) #)
  IN _ -> (# | (# #) #)

-- | Integer->Int16 via IS/IP/IN
toInt16# :: Integer -> (# Int16 | (# #) #)
toInt16# x = case x of
  IS i# ->
    let n# = intToInt16# i#
    in case int16ToInt# n# ==# i# of
      1# -> (# I16# n# | #)
      _  -> (# | (# #) #)
  IP _ -> (# | (# #) #)
  IN _ -> (# | (# #) #)

-- | Integer->Int32 via IS/IP/IN
toInt32# :: Integer -> (# Int32 | (# #) #)
toInt32# x = case x of
  IS i# ->
    let n# = intToInt32# i#
    in case int32ToInt# n# ==# i# of
      1# -> (# I32# n# | #)
      _  -> (# | (# #) #)
  IP _ -> (# | (# #) #)
  IN _ -> (# | (# #) #)

-- | Integer->Int64 via IS/IP/IN
toInt64# :: Integer -> (# Int64 | (# #) #)
toInt64# x = case x of
  IS i# -> (# I64# (intToInt64# i#) | #)
  IP _ -> (# | (# #) #)
  IN _ -> (# | (# #) #)

-- | Integer->Int via IS/IP/IN
toInt# :: Integer -> (# Int | (# #) #)
toInt# x = case x of
  IS i# -> (# I# i# | #)
  IP _ -> (# | (# #) #)
  IN _ -> (# | (# #) #)

-- | Integer->Word8, IS case uses signed->unsigned narrow
toWord8# :: Integer -> (# Word8 | (# #) #)
toWord8# x = case x of
  IS i# ->
    let n# = wordToWord8# (int2Word# i#)
    in case word2Int# (word8ToWord# n#) ==# i# of
      1# -> (# W8# n# | #)
      _  -> (# | (# #) #)
  IP _ -> (# | (# #) #)
  IN _ -> (# | (# #) #)

-- | Integer->Word16
toWord16# :: Integer -> (# Word16 | (# #) #)
toWord16# x = case x of
  IS i# ->
    let n# = wordToWord16# (int2Word# i#)
    in case word2Int# (word16ToWord# n#) ==# i# of
      1# -> (# W16# n# | #)
      _  -> (# | (# #) #)
  IP _ -> (# | (# #) #)
  IN _ -> (# | (# #) #)

-- | Integer->Word32
toWord32# :: Integer -> (# Word32 | (# #) #)
toWord32# x = case x of
  IS i# ->
    let n# = wordToWord32# (int2Word# i#)
    in case word2Int# (word32ToWord# n#) ==# i# of
      1# -> (# W32# n# | #)
      _  -> (# | (# #) #)
  IP _ -> (# | (# #) #)
  IN _ -> (# | (# #) #)

-- | Integer->Word64: IS checks non-negative; IP uses integerToWord# roundtrip
toWord64# :: Integer -> (# Word64 | (# #) #)
toWord64# x = case x of
  IS i# -> case i# >=# 0# of
    1# -> (# W64# (wordToWord64# (int2Word# i#)) | #)
    _  -> (# | (# #) #)
  IP _ ->
    let w# = integerToWord# x
    in case integerEq# (integerFromWord# w#) x of
      1# -> (# W64# (wordToWord64# w#) | #)
      _  -> (# | (# #) #)
  IN _ -> (# | (# #) #)

-- | Integer->Word: IS checks non-negative; IP uses integerToWord# roundtrip
toWord# :: Integer -> (# Word | (# #) #)
toWord# x = case x of
  IS i# -> case i# >=# 0# of
    1# -> (# W# (int2Word# i#) | #)
    _  -> (# | (# #) #)
  IP _ ->
    let w# = integerToWord# x
    in case integerEq# (integerFromWord# w#) x of
      1# -> (# W# w# | #)
      _  -> (# | (# #) #)
  IN _ -> (# | (# #) #)
