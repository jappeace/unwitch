-- | Conversions from lazy 'Data.Text.Lazy.Text'.
module Unwitch.Convert.LazyText
  ( toText
  , toString
  , fromString
  , toLazyByteStringUtf8
  , toLazyByteStringUtf16LE
  , toLazyByteStringUtf16BE
  , toLazyByteStringUtf32LE
  , toLazyByteStringUtf32BE
  , toLazyByteStringLatin1
  )
where

import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBSC8
import Data.Text (Text)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE

-- | Converts lazy 'Data.Text.Lazy.Text' to strict 'Text'.
toText :: LT.Text -> Text
toText = LT.toStrict

-- | Unpacks lazy 'Data.Text.Lazy.Text' to a 'String'.
toString :: LT.Text -> String
toString = LT.unpack

-- | Packs a 'String' into lazy 'Data.Text.Lazy.Text'.
fromString :: String -> LT.Text
fromString = LT.pack

-- | Encodes lazy 'Data.Text.Lazy.Text' as a UTF-8 lazy 'Data.ByteString.Lazy.ByteString'.
toLazyByteStringUtf8 :: LT.Text -> LBS.ByteString
toLazyByteStringUtf8 = LTE.encodeUtf8

-- | Encodes as UTF-16 little-endian lazy 'Data.ByteString.Lazy.ByteString'.
toLazyByteStringUtf16LE :: LT.Text -> LBS.ByteString
toLazyByteStringUtf16LE = LTE.encodeUtf16LE

-- | Encodes as UTF-16 big-endian lazy 'Data.ByteString.Lazy.ByteString'.
toLazyByteStringUtf16BE :: LT.Text -> LBS.ByteString
toLazyByteStringUtf16BE = LTE.encodeUtf16BE

-- | Encodes as UTF-32 little-endian lazy 'Data.ByteString.Lazy.ByteString'.
toLazyByteStringUtf32LE :: LT.Text -> LBS.ByteString
toLazyByteStringUtf32LE = LTE.encodeUtf32LE

-- | Encodes as UTF-32 big-endian lazy 'Data.ByteString.Lazy.ByteString'.
toLazyByteStringUtf32BE :: LT.Text -> LBS.ByteString
toLazyByteStringUtf32BE = LTE.encodeUtf32BE

-- | Encodes as Latin-1 lazy 'Data.ByteString.Lazy.ByteString', returns 'Nothing' if any character is above @\xFF@.
toLazyByteStringLatin1 :: LT.Text -> Maybe LBS.ByteString
toLazyByteStringLatin1 t = if LT.all isLatin1 t
  then Just $ LBSC8.pack (LT.unpack t)
  else Nothing

isLatin1 :: Char -> Bool
isLatin1 c = c <= '\xFF'
