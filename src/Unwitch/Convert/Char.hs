-- | Conversions from 'Char'.
module Unwitch.Convert.Char
  ( -- * Conversions
    toInt
  , toWord
  , fromInt
  , fromWord
  -- * Unboxed conversions
  -- $unboxed
  , fromInt#
  , fromWord#
  )
where

import Data.Char (ord, chr)
import GHC.Exts (Int(..), Word(..), Char(..), chr#,
                 word2Int#, (>=#), (<=#), leWord#, gtWord#)

-- $unboxed
-- These use GHC unboxed types and unboxed sums for zero-allocation
-- failure handling. Requires the @MagicHash@, @UnboxedSums@ and
-- @UnboxedTuples@ language extensions.
-- See the <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/primitives.html GHC manual on unboxed types>.

-- | Converts a Char to its Unicode codepoint as Int. Infallible.
toInt :: Char -> Int
toInt = ord

-- | Converts a Char to its Unicode codepoint as Word. Infallible.
toWord :: Char -> Word
toWord = fromIntegral . ord

-- | Converts an Int to a Char if it is a valid Unicode codepoint.
-- Valid range: 0..0xD7FF and 0xE000..0x10FFFF (excludes surrogates).
fromInt :: Int -> Maybe Char
fromInt i = if isValidCodepoint (fromIntegral i)
  then Just $ chr i
  else Nothing

-- | Unboxed variant of 'fromInt'. Checks valid Unicode codepoint range.
fromInt# :: Int -> (# Char | (# #) #)
fromInt# (I# i#) = case i# >=# 0# of
  1# -> case i# <=# 0xD7FF# of
    1# -> (# C# (chr# i#) | #)
    _  -> case i# >=# 0xE000# of
      1# -> case i# <=# 0x10FFFF# of
        1# -> (# C# (chr# i#) | #)
        _  -> (# | (# #) #)
      _  -> (# | (# #) #)
  _  -> (# | (# #) #)

-- | Converts a Word to a Char if it is a valid Unicode codepoint.
-- Valid range: 0..0xD7FF and 0xE000..0x10FFFF (excludes surrogates).
fromWord :: Word -> Maybe Char
fromWord w = if isValidCodepoint (fromIntegral w)
  then Just $ chr (fromIntegral w)
  else Nothing

-- | Unboxed variant of 'fromWord'. Checks valid Unicode codepoint range.
fromWord# :: Word -> (# Char | (# #) #)
fromWord# (W# w#) = case leWord# w# 0xD7FF## of
  1# -> (# C# (chr# (word2Int# w#)) | #)
  _  -> case gtWord# w# 0xDFFF## of
    1# -> case leWord# w# 0x10FFFF## of
      1# -> (# C# (chr# (word2Int# w#)) | #)
      _  -> (# | (# #) #)
    _  -> (# | (# #) #)

isValidCodepoint :: Integer -> Bool
isValidCodepoint cp =
  (cp >= 0 && cp <= 0xD7FF) || (cp >= 0xE000 && cp <= 0x10FFFF)
