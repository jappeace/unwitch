-- | Conversions from 'Data.Text.Text'.
module Unwitch.Convert.Text
  ( toLazyText
  , toString
  , fromString
  , toByteStringUtf8
  , toByteStringUtf16LE
  , toByteStringUtf16BE
  , toByteStringUtf32LE
  , toByteStringUtf32BE
  , toByteStringLatin1
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC8
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as LT

-- | Converts strict 'Text' to lazy 'Data.Text.Lazy.Text'.
toLazyText :: Text -> LT.Text
toLazyText = LT.fromStrict

-- | Unpacks 'Text' to a 'String'.
toString :: Text -> String
toString = T.unpack

-- | Packs a 'String' into 'Text'.
fromString :: String -> Text
fromString = T.pack

-- | Encodes 'Text' as a UTF-8 'ByteString'.
toByteStringUtf8 :: Text -> ByteString
toByteStringUtf8 = TE.encodeUtf8

-- | Encodes 'Text' as a UTF-16 little-endian 'ByteString'.
toByteStringUtf16LE :: Text -> ByteString
toByteStringUtf16LE = TE.encodeUtf16LE

-- | Encodes 'Text' as a UTF-16 big-endian 'ByteString'.
toByteStringUtf16BE :: Text -> ByteString
toByteStringUtf16BE = TE.encodeUtf16BE

-- | Encodes 'Text' as a UTF-32 little-endian 'ByteString'.
toByteStringUtf32LE :: Text -> ByteString
toByteStringUtf32LE = TE.encodeUtf32LE

-- | Encodes 'Text' as a UTF-32 big-endian 'ByteString'.
toByteStringUtf32BE :: Text -> ByteString
toByteStringUtf32BE = TE.encodeUtf32BE

-- | Encodes 'Text' as a Latin-1 'ByteString', returns 'Nothing' if any character is above @\xFF@.
toByteStringLatin1 :: Text -> Maybe ByteString
toByteStringLatin1 t = if T.all isLatin1 t
  then Just $ BSC8.pack (T.unpack t)
  else Nothing

isLatin1 :: Char -> Bool
isLatin1 c = c <= '\xFF'

