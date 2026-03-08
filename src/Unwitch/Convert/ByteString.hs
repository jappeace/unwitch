module Unwitch.Convert.ByteString
  ( toLazyByteString
  , toShortByteString
  , toWord8s
  , fromWord8s
  , toTextLatin1
  , toTextUtf8
  , toTextUtf8#
  , toTextUtf16LE
  , toTextUtf16LE#
  , toTextUtf16BE
  , toTextUtf16BE#
  , toTextUtf32LE
  , toTextUtf32LE#
  , toTextUtf32BE
  , toTextUtf32BE#
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

toTextUtf8# :: ByteString -> (# UnicodeException | Text #)
toTextUtf8# x = case toTextUtf8 x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toTextUtf16LE# :: ByteString -> (# UnicodeException | Text #)
toTextUtf16LE# x = case toTextUtf16LE x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toTextUtf16BE# :: ByteString -> (# UnicodeException | Text #)
toTextUtf16BE# x = case toTextUtf16BE x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toTextUtf32LE# :: ByteString -> (# UnicodeException | Text #)
toTextUtf32LE# x = case toTextUtf32LE x of
  Left e  -> (# e | #)
  Right y -> (# | y #)

toTextUtf32BE# :: ByteString -> (# UnicodeException | Text #)
toTextUtf32BE# x = case toTextUtf32BE x of
  Left e  -> (# e | #)
  Right y -> (# | y #)
