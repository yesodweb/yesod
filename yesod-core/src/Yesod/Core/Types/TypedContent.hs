{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Core.Types.TypedContent
  (
    ContentType
  , TypedContent (..)
  , typedContentToSnippet
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void, guard)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as BB

import qualified Data.Int as I

#if MIN_VERSION_text(2,1,0)
import qualified Data.Text.Encoding as TE (decodeASCIIPrefix)
#else
import qualified Data.Text.Encoding as TE (decodeLatin1)
#endif
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as LE (decodeUtf8With, decodeLatin1)
import qualified Data.Text.Encoding.Error as EE (lenientDecode)

import qualified Data.Encoding as Enc
import qualified Data.Encoding.GB18030 as Enc
import qualified Data.Encoding.CP1251 as Enc
import qualified Data.Encoding.ShiftJIS as Enc
import qualified Data.Encoding.CP932 as Enc

import qualified Network.Wai.Parse as NWP

import Yesod.Core.Types.Content (Content (..))

type ContentType = B.ByteString -- FIXME Text?
data TypedContent = TypedContent !ContentType !Content

decoderForCharset :: Maybe B.ByteString -> L.ByteString -> TL.Text
decoderForCharset (Just encodingSymbol)
  | encodingSymbol == "utf-8" =
      LE.decodeUtf8With EE.lenientDecode
  | encodingSymbol == "US-ASCII" =
#if MIN_VERSION_text(2,1,0)
      TL.fromStrict . fst . TE.decodeASCIIPrefix . B.toStrict
#else
      TL.fromStrict . TE.decodeLatin1 . B.toStrict
#endif
  | encodingSymbol == "latin1" =
      LE.decodeLatin1
  | encodingSymbol == "GB18030" =
      TL.pack . Enc.decodeLazyByteString Enc.GB18030
  | encodingSymbol == "windows-1251" =
      TL.pack . Enc.decodeLazyByteString Enc.CP1251
  | encodingSymbol == "Shift_JIS" =
      TL.pack . Enc.decodeLazyByteString Enc.ShiftJIS
  | encodingSymbol == "Windows-31J" =
      TL.pack . Enc.decodeLazyByteString Enc.CP932
  | otherwise =
      LE.decodeUtf8With EE.lenientDecode
decoderForCharset Nothing = LE.decodeUtf8With EE.lenientDecode

decodeForContentType :: ContentType -> L.ByteString -> Maybe TL.Text
decodeForContentType ct bytes = do
  let (t, params) =
        NWP.parseContentType ct
      charset =
        lookup "charset" params
      typeIsText =
        B.isPrefixOf "text" t
            || B.isPrefixOf "application/json" t
            || B.isPrefixOf "application/rss"  t
            || B.isPrefixOf "application/atom" t
      decoder = decoderForCharset charset
  void charset <|> guard typeIsText
  pure $ decoder bytes

contentToSnippet :: Content -> I.Int64 -> Maybe L.ByteString
contentToSnippet (ContentBuilder builder maybeLength) maxLength =
  pure $ truncatedText <> excessLengthMsg
  where
    truncatedText = L.take maxLength $ BB.toLazyByteString builder
    excessLength = fromMaybe 0 $ (subtract $ fromIntegral maxLength) <$> maybeLength
    excessLengthMsg = case excessLength > 0 of
      False -> ""
      True -> "...+ " <> BB.toLazyByteString (BB.intDec excessLength)
contentToSnippet (ContentSource _) _ = Nothing
contentToSnippet (ContentFile _ _) _ = Nothing
contentToSnippet (ContentDontEvaluate _) _ = Nothing

-- | Represents TypedContent as a String, rendering at most a specified number of
-- bytes of the content, and annotating it with the remaining length. Returns Nothing
-- if the content type indicates the content is binary data.
--
-- @since 1.6.28.0
typedContentToSnippet :: TypedContent -> I.Int64 -> Maybe TL.Text
typedContentToSnippet (TypedContent t c) maxLength = decodeForContentType t =<< contentToSnippet c maxLength
