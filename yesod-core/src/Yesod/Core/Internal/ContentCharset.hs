{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Decoders for charsets that rely on the @encoding@ package.
--
-- This module isolates the only @encoding@-related CPP in yesod-core. The
-- @encoding@ package does not build on Windows (it pulls in
-- @regex-compat@/@regex-posix@ and needs @system_encoding.h@), so the cabal
-- file gates it behind @WITH_DATA_ENCODING@. Callers stay CPP-free: they ask
-- 'lookupExtraCharsetDecoder' for a decoder and fall back when it is
-- 'Nothing' — which is what happens on Windows for these charsets.
module Yesod.Core.Internal.ContentCharset
  ( lookupExtraCharsetDecoder
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as TL

#ifdef WITH_DATA_ENCODING
import qualified Data.Encoding as Enc
import qualified Data.Encoding.GB18030 as Enc
import qualified Data.Encoding.CP1251 as Enc
import qualified Data.Encoding.ShiftJIS as Enc
import qualified Data.Encoding.CP932 as Enc
#endif

-- | Look up a decoder for a charset that is only supported when the
-- @encoding@ package is available. Returns 'Nothing' for charsets handled by
-- the caller directly (utf-8, US-ASCII, latin1), for unrecognized charsets,
-- and — on Windows — for the CJK charsets below, so the caller can fall back
-- to utf-8-lenient decoding.
lookupExtraCharsetDecoder :: B.ByteString -> Maybe (L.ByteString -> TL.Text)
#ifdef WITH_DATA_ENCODING
lookupExtraCharsetDecoder encodingSymbol
  | encodingSymbol == "GB18030" =
      Just $ TL.pack . Enc.decodeLazyByteString Enc.GB18030
  | encodingSymbol == "windows-1251" =
      Just $ TL.pack . Enc.decodeLazyByteString Enc.CP1251
  | encodingSymbol == "Shift_JIS" =
      Just $ TL.pack . Enc.decodeLazyByteString Enc.ShiftJIS
  | encodingSymbol == "Windows-31J" =
      Just $ TL.pack . Enc.decodeLazyByteString Enc.CP932
  | otherwise = Nothing
#else
lookupExtraCharsetDecoder _ = Nothing
#endif
