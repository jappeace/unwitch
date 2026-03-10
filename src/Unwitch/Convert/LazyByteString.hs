-- | Conversions from lazy 'Data.ByteString.Lazy.ByteString'.
module Unwitch.Convert.LazyByteString
  ( toByteString
  , toWord8s
  , fromWord8s
  , toLazyTextLatin1
  , toLazyTextUtf8
  , toLazyTextUtf16LE
  , toLazyTextUtf16BE
  , toLazyTextUtf32LE
  , toLazyTextUtf32BE
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import Data.Text.Encoding.Error (UnicodeException)
import Data.Word (Word8)
import Unwitch.TryEvaluate (tryEvaluate)

-- | Converts lazy 'Data.ByteString.Lazy.ByteString' to strict 'ByteString'.
toByteString :: LBS.ByteString -> ByteString
toByteString = LBS.toStrict

-- | Unpacks a lazy 'Data.ByteString.Lazy.ByteString' to a list of bytes.
toWord8s :: LBS.ByteString -> [Word8]
toWord8s = LBS.unpack

-- | Packs a list of bytes into a lazy 'Data.ByteString.Lazy.ByteString'.
fromWord8s :: [Word8] -> LBS.ByteString
fromWord8s = LBS.pack

-- | Decodes as Latin-1 to lazy 'Data.Text.Lazy.Text'. Infallible.
toLazyTextLatin1 :: LBS.ByteString -> LT.Text
toLazyTextLatin1 = LTE.decodeLatin1

-- | Decodes as UTF-8 to lazy 'Data.Text.Lazy.Text'.
toLazyTextUtf8 :: LBS.ByteString -> Either UnicodeException LT.Text
toLazyTextUtf8 = LTE.decodeUtf8'

-- | Decodes as UTF-16 little-endian to lazy 'Data.Text.Lazy.Text'.
toLazyTextUtf16LE :: LBS.ByteString -> Either UnicodeException LT.Text
toLazyTextUtf16LE = tryEvaluate . LTE.decodeUtf16LE

-- | Decodes as UTF-16 big-endian to lazy 'Data.Text.Lazy.Text'.
toLazyTextUtf16BE :: LBS.ByteString -> Either UnicodeException LT.Text
toLazyTextUtf16BE = tryEvaluate . LTE.decodeUtf16BE

-- | Decodes as UTF-32 little-endian to lazy 'Data.Text.Lazy.Text'.
toLazyTextUtf32LE :: LBS.ByteString -> Either UnicodeException LT.Text
toLazyTextUtf32LE = tryEvaluate . LTE.decodeUtf32LE

-- | Decodes as UTF-32 big-endian to lazy 'Data.Text.Lazy.Text'.
toLazyTextUtf32BE :: LBS.ByteString -> Either UnicodeException LT.Text
toLazyTextUtf32BE = tryEvaluate . LTE.decodeUtf32BE

