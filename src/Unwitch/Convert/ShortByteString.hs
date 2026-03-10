-- | Conversions from 'Data.ByteString.Short.ShortByteString'.
module Unwitch.Convert.ShortByteString
  ( toByteString
  , toWord8s
  , fromWord8s
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Word (Word8)

-- | Converts 'ShortByteString' to strict 'ByteString'.
toByteString :: ShortByteString -> ByteString
toByteString = SBS.fromShort

-- | Unpacks a 'ShortByteString' to a list of bytes.
toWord8s :: ShortByteString -> [Word8]
toWord8s = SBS.unpack

-- | Packs a list of bytes into a 'ShortByteString'.
fromWord8s :: [Word8] -> ShortByteString
fromWord8s = SBS.pack
