-- | Conversions from 'Data.ByteString.ByteString'.
module Unwitch.Convert.ByteString
  ( toLazyByteString
  , toShortByteString
  , toWord8s
  , fromWord8s
  , toTextLatin1
  , toTextUtf8
  , toTextUtf16LE
  , toTextUtf16BE
  , toTextUtf32LE
  , toTextUtf32BE
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.ByteString.Short (ShortByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (UnicodeException)
import Data.Word (Word8)
import Unwitch.TryEvaluate (tryEvaluate)

-- | Converts strict 'ByteString' to lazy 'Data.ByteString.Lazy.ByteString'.
toLazyByteString :: ByteString -> LBS.ByteString
toLazyByteString = LBS.fromStrict

-- | Converts 'ByteString' to 'ShortByteString'.
toShortByteString :: ByteString -> ShortByteString
toShortByteString = SBS.toShort

-- | Unpacks a 'ByteString' to a list of bytes.
toWord8s :: ByteString -> [Word8]
toWord8s = BS.unpack

-- | Packs a list of bytes into a 'ByteString'.
fromWord8s :: [Word8] -> ByteString
fromWord8s = BS.pack

-- | Decodes a 'ByteString' as Latin-1 to 'Text'. Infallible.
toTextLatin1 :: ByteString -> Text
toTextLatin1 = TE.decodeLatin1

-- | Decodes a 'ByteString' as UTF-8 to 'Text'.
toTextUtf8 :: ByteString -> Either UnicodeException Text
toTextUtf8 = TE.decodeUtf8'

-- | Decodes a 'ByteString' as UTF-16 little-endian to 'Text'.
toTextUtf16LE :: ByteString -> Either UnicodeException Text
toTextUtf16LE = tryEvaluate . TE.decodeUtf16LE

-- | Decodes a 'ByteString' as UTF-16 big-endian to 'Text'.
toTextUtf16BE :: ByteString -> Either UnicodeException Text
toTextUtf16BE = tryEvaluate . TE.decodeUtf16BE

-- | Decodes a 'ByteString' as UTF-32 little-endian to 'Text'.
toTextUtf32LE :: ByteString -> Either UnicodeException Text
toTextUtf32LE = tryEvaluate . TE.decodeUtf32LE

-- | Decodes a 'ByteString' as UTF-32 big-endian to 'Text'.
toTextUtf32BE :: ByteString -> Either UnicodeException Text
toTextUtf32BE = tryEvaluate . TE.decodeUtf32BE

