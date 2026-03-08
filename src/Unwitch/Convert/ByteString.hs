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

toLazyByteString :: ByteString -> LBS.ByteString
toLazyByteString = LBS.fromStrict

toShortByteString :: ByteString -> ShortByteString
toShortByteString = SBS.toShort

toWord8s :: ByteString -> [Word8]
toWord8s = BS.unpack

fromWord8s :: [Word8] -> ByteString
fromWord8s = BS.pack

toTextLatin1 :: ByteString -> Text
toTextLatin1 = TE.decodeLatin1

toTextUtf8 :: ByteString -> Either UnicodeException Text
toTextUtf8 = TE.decodeUtf8'

toTextUtf16LE :: ByteString -> Either UnicodeException Text
toTextUtf16LE = tryEvaluate . TE.decodeUtf16LE

toTextUtf16BE :: ByteString -> Either UnicodeException Text
toTextUtf16BE = tryEvaluate . TE.decodeUtf16BE

toTextUtf32LE :: ByteString -> Either UnicodeException Text
toTextUtf32LE = tryEvaluate . TE.decodeUtf32LE

toTextUtf32BE :: ByteString -> Either UnicodeException Text
toTextUtf32BE = tryEvaluate . TE.decodeUtf32BE

