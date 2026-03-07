module Unwitch.Convert.Char
  ( toInt
  , toWord
  , fromInt
  , fromWord
  )
where

import Data.Char (ord, chr)

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

-- | Converts a Word to a Char if it is a valid Unicode codepoint.
-- Valid range: 0..0xD7FF and 0xE000..0x10FFFF (excludes surrogates).
fromWord :: Word -> Maybe Char
fromWord w = if isValidCodepoint (fromIntegral w)
  then Just $ chr (fromIntegral w)
  else Nothing

isValidCodepoint :: Integer -> Bool
isValidCodepoint cp =
  (cp >= 0 && cp <= 0xD7FF) || (cp >= 0xE000 && cp <= 0x10FFFF)
