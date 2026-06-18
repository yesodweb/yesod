{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Decoders for charsets that rely on the @encoding@ package.
--
-- This module isolates the only @encoding@-related CPP in yesod-core. The
-- @encoding@ package does not build on Windows (it pulls in
-- @regex-compat@/@regex-posix@ and needs @system_encoding.h@), so the cabal
-- file gates it behind @WITH_DATA_ENCODING@. Callers stay CPP-free: they hand
-- a charset to 'extraCharsetDecoder' and use whatever decoder it returns.
module Yesod.Core.Internal.ContentCharset
  ( extraCharsetDecoder
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as LE (decodeUtf8With)
import qualified Data.Text.Encoding.Error as EE (lenientDecode)

#ifdef WITH_DATA_ENCODING
import qualified Data.Encoding as Enc
import qualified Data.Encoding.GB18030 as Enc
import qualified Data.Encoding.CP1251 as Enc
import qualified Data.Encoding.ShiftJIS as Enc
import qualified Data.Encoding.CP932 as Enc
#endif

-- | Decoder for charsets beyond the ones the caller handles directly (utf-8,
-- US-ASCII, latin1). It recognizes the CJK charsets that need the @encoding@
-- package; for any other charset — and for those charsets on Windows, where
-- @encoding@ is unavailable — it falls back to utf-8-lenient decoding.
extraCharsetDecoder :: B.ByteString -> L.ByteString -> TL.Text
#ifdef WITH_DATA_ENCODING
extraCharsetDecoder encodingSymbol
  | encodingSymbol == "GB18030" =
      TL.pack . Enc.decodeLazyByteString Enc.GB18030
  | encodingSymbol == "windows-1251" =
      TL.pack . Enc.decodeLazyByteString Enc.CP1251
  | encodingSymbol == "Shift_JIS" =
      TL.pack . Enc.decodeLazyByteString Enc.ShiftJIS
  | encodingSymbol == "Windows-31J" =
      TL.pack . Enc.decodeLazyByteString Enc.CP932
  | otherwise = lenientUtf8
#else
extraCharsetDecoder _ = lenientUtf8
#endif

-- | The default decoder used when a charset is unrecognized or unsupported.
lenientUtf8 :: L.ByteString -> TL.Text
lenientUtf8 = LE.decodeUtf8With EE.lenientDecode
