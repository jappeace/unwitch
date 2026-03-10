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

toText :: LT.Text -> Text
toText = LT.toStrict

toString :: LT.Text -> String
toString = LT.unpack

fromString :: String -> LT.Text
fromString = LT.pack

toLazyByteStringUtf8 :: LT.Text -> LBS.ByteString
toLazyByteStringUtf8 = LTE.encodeUtf8

toLazyByteStringUtf16LE :: LT.Text -> LBS.ByteString
toLazyByteStringUtf16LE = LTE.encodeUtf16LE

toLazyByteStringUtf16BE :: LT.Text -> LBS.ByteString
toLazyByteStringUtf16BE = LTE.encodeUtf16BE

toLazyByteStringUtf32LE :: LT.Text -> LBS.ByteString
toLazyByteStringUtf32LE = LTE.encodeUtf32LE

toLazyByteStringUtf32BE :: LT.Text -> LBS.ByteString
toLazyByteStringUtf32BE = LTE.encodeUtf32BE

-- | Returns 'Nothing' if any character exceeds @\xFF@.
toLazyByteStringLatin1 :: LT.Text -> Maybe LBS.ByteString
toLazyByteStringLatin1 t = if LT.all isLatin1 t
  then Just $ LBSC8.pack (LT.unpack t)
  else Nothing

isLatin1 :: Char -> Bool
isLatin1 c = c <= '\xFF'
