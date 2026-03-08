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

toLazyText :: Text -> LT.Text
toLazyText = LT.fromStrict

toString :: Text -> String
toString = T.unpack

fromString :: String -> Text
fromString = T.pack

toByteStringUtf8 :: Text -> ByteString
toByteStringUtf8 = TE.encodeUtf8

toByteStringUtf16LE :: Text -> ByteString
toByteStringUtf16LE = TE.encodeUtf16LE

toByteStringUtf16BE :: Text -> ByteString
toByteStringUtf16BE = TE.encodeUtf16BE

toByteStringUtf32LE :: Text -> ByteString
toByteStringUtf32LE = TE.encodeUtf32LE

toByteStringUtf32BE :: Text -> ByteString
toByteStringUtf32BE = TE.encodeUtf32BE

toByteStringLatin1 :: Text -> Maybe ByteString
toByteStringLatin1 t = if T.all isLatin1 t
  then Just $ BSC8.pack (T.unpack t)
  else Nothing

isLatin1 :: Char -> Bool
isLatin1 c = c <= '\xFF'

