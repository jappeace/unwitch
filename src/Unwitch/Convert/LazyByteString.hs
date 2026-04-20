-- | Conversions from lazy 'Data.ByteString.Lazy.ByteString'.
module Unwitch.Convert.LazyByteString
  ( toByteString
  , toWord8s
  , fromWord8s
#ifdef __GLASGOW_HASKELL__
  , toLazyTextLatin1
  , toLazyTextUtf8
  , toLazyTextUtf16LE
  , toLazyTextUtf16BE
  , toLazyTextUtf32LE
  , toLazyTextUtf32BE
#endif
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Word (Word8)
#ifdef __GLASGOW_HASKELL__
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LTE
import Data.Text.Encoding.Error (UnicodeException)
import Unwitch.TryEvaluate (tryEvaluate)
#endif

toByteString :: LBS.ByteString -> ByteString
toByteString = LBS.toStrict

toWord8s :: LBS.ByteString -> [Word8]
toWord8s = LBS.unpack

fromWord8s :: [Word8] -> LBS.ByteString
fromWord8s = LBS.pack

#ifdef __GLASGOW_HASKELL__
toLazyTextLatin1 :: LBS.ByteString -> LT.Text
toLazyTextLatin1 = LTE.decodeLatin1

toLazyTextUtf8 :: LBS.ByteString -> Either UnicodeException LT.Text
toLazyTextUtf8 = LTE.decodeUtf8'

toLazyTextUtf16LE :: LBS.ByteString -> Either UnicodeException LT.Text
toLazyTextUtf16LE = tryEvaluate . LTE.decodeUtf16LE

toLazyTextUtf16BE :: LBS.ByteString -> Either UnicodeException LT.Text
toLazyTextUtf16BE = tryEvaluate . LTE.decodeUtf16BE

toLazyTextUtf32LE :: LBS.ByteString -> Either UnicodeException LT.Text
toLazyTextUtf32LE = tryEvaluate . LTE.decodeUtf32LE

toLazyTextUtf32BE :: LBS.ByteString -> Either UnicodeException LT.Text
toLazyTextUtf32BE = tryEvaluate . LTE.decodeUtf32BE
#endif
