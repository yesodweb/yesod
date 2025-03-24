{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Core.Content
    ( -- * Content
      Content (..)
    , emptyContent
    , ToContent (..)
    , ToFlushBuilder (..)
      -- * Mime types
      -- ** Data type
    , ContentType
    , typeHtml
    , typePlain
    , typeJson
    , typeXml
    , typeAtom
    , typeRss
    , typeJpeg
    , typePng
    , typeGif
    , typeSvg
    , typeJavascript
    , typeCss
    , typeFlv
    , typeOgv
    , typeOctet
      -- * Utilities
    , simpleContentType
    , contentTypeTypes
    , typedContentToSnippet
      -- * Evaluation strategy
    , DontFullyEvaluate (..)
      -- * Representations
    , TypedContent (..)
    , ToTypedContent (..)
    , HasContentType (..)
      -- ** Specific content types
    , RepHtml
    , RepJson (..)
    , RepPlain (..)
    , RepXml (..)
      -- ** Smart constructors
    , repJson
    , repPlain
    , repXml
    ) where

import Data.Maybe
import Control.Applicative

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as BB
import Data.Text.Lazy (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8Builder, encodeUtf8, decodeASCIIPrefix)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as LE (decodeUtf8With, decodeLatin1)
import Data.Text.Encoding.Error as EE (lenientDecode)

import Data.ByteString.Builder (Builder, byteString, lazyByteString, stringUtf8)
import Text.Hamlet (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Data.Conduit (Flush (Chunk), SealedConduitT, mapOutput)
import Control.Monad (liftM, guard)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Conduit.Internal as CI

import qualified Data.Aeson as J
import qualified Data.Encoding as Enc
import qualified Data.Encoding.GB18030 as Enc
import qualified Data.Encoding.CP1251 as Enc
import qualified Data.Encoding.ShiftJIS as Enc
import qualified Data.Encoding.CP932 as Enc

import Data.Text.Lazy.Builder (toLazyText)
import Data.Void (Void, absurd)
import Yesod.Core.Types
import Text.Lucius (Css, renderCss)
import Text.Julius (Javascript, unJavascript)
import qualified Network.Wai.Parse as NWP
import qualified GHC.Int as I
import Data.Word8 (_semicolon, _slash)
import Control.Arrow (second)
import Control.Exception (Exception)

-- | Zero-length enumerator.
emptyContent :: Content
emptyContent = ContentBuilder mempty $ Just 0

-- | Anything which can be converted into 'Content'. Most of the time, you will
-- want to use the 'ContentBuilder' constructor. An easier approach will be to use
-- a pre-defined 'toContent' function, such as converting your data into a lazy
-- bytestring and then calling 'toContent' on that.
--
-- Please note that the built-in instances for lazy data structures ('String',
-- lazy 'L.ByteString', lazy 'Text' and 'Html') will not automatically include
-- the content length for the 'ContentBuilder' constructor.
class ToContent a where
    toContent :: a -> Content

instance ToContent Content where
    toContent = id
instance ToContent Builder where
    toContent = flip ContentBuilder Nothing
instance ToContent B.ByteString where
    toContent bs = ContentBuilder (byteString bs) $ Just $ B.length bs
instance ToContent L.ByteString where
    toContent = flip ContentBuilder Nothing . lazyByteString
instance ToContent T.Text where
    toContent = toContent . encodeUtf8Builder
instance ToContent Text where
    toContent = toContent . foldMap encodeUtf8Builder . TL.toChunks
instance ToContent String where
    toContent = toContent . stringUtf8
instance ToContent Html where
    toContent bs = ContentBuilder (renderHtmlBuilder bs) Nothing
instance ToContent () where
    toContent () = toContent B.empty
instance ToContent Void where
    toContent = absurd
instance ToContent (ContentType, Content) where
    toContent = snd
instance ToContent TypedContent where
    toContent (TypedContent _ c) = c
instance ToContent (JSONResponse a) where
    toContent (JSONResponse a) = toContent $ J.toEncoding a

instance ToContent Css where
    toContent = toContent . renderCss
instance ToContent Javascript where
    toContent = toContent . toLazyText . unJavascript

instance ToFlushBuilder builder => ToContent (CI.Pipe () () builder () (ResourceT IO) ()) where
    toContent src = ContentSource $ CI.ConduitT (CI.mapOutput toFlushBuilder src >>=)

instance ToFlushBuilder builder => ToContent (CI.ConduitT () builder (ResourceT IO) ()) where
    toContent src = ContentSource $ mapOutput toFlushBuilder src
instance ToFlushBuilder builder => ToContent (SealedConduitT () builder (ResourceT IO) ()) where
    toContent (CI.SealedConduitT src) = toContent src

-- | A class for all data which can be sent in a streaming response. Note that
-- for textual data, instances must use UTF-8 encoding.
--
-- Since 1.2.0
class ToFlushBuilder a where toFlushBuilder :: a -> Flush Builder
instance ToFlushBuilder (Flush Builder) where toFlushBuilder = id
instance ToFlushBuilder Builder where toFlushBuilder = Chunk
instance ToFlushBuilder (Flush B.ByteString) where toFlushBuilder = fmap byteString
instance ToFlushBuilder B.ByteString where toFlushBuilder = Chunk . byteString
instance ToFlushBuilder (Flush L.ByteString) where toFlushBuilder = fmap lazyByteString
instance ToFlushBuilder L.ByteString where toFlushBuilder = Chunk . lazyByteString
instance ToFlushBuilder (Flush Text) where toFlushBuilder = fmap (foldMap encodeUtf8Builder . TL.toChunks)
instance ToFlushBuilder Text where toFlushBuilder = Chunk . foldMap encodeUtf8Builder . TL.toChunks
instance ToFlushBuilder (Flush T.Text) where toFlushBuilder = fmap encodeUtf8Builder
instance ToFlushBuilder T.Text where toFlushBuilder = Chunk . encodeUtf8Builder
instance ToFlushBuilder (Flush String) where toFlushBuilder = fmap stringUtf8
instance ToFlushBuilder String where toFlushBuilder = Chunk . stringUtf8
instance ToFlushBuilder (Flush Html) where toFlushBuilder = fmap renderHtmlBuilder
instance ToFlushBuilder Html where toFlushBuilder = Chunk . renderHtmlBuilder

repJson :: ToContent a => a -> RepJson
repJson = RepJson . toContent

repPlain :: ToContent a => a -> RepPlain
repPlain = RepPlain . toContent

repXml :: ToContent a => a -> RepXml
repXml = RepXml . toContent

class ToTypedContent a => HasContentType a where
    getContentType :: Monad m => m a -> ContentType

instance HasContentType RepJson where
    getContentType _ = typeJson
deriving instance ToContent RepJson

instance HasContentType RepPlain where
    getContentType _ = typePlain
deriving instance ToContent RepPlain
instance HasContentType (JSONResponse a) where
    getContentType _ = typeJson

instance HasContentType RepXml where
    getContentType _ = typeXml
deriving instance ToContent RepXml


typeHtml :: ContentType
typeHtml = "text/html; charset=utf-8"

typePlain :: ContentType
typePlain = "text/plain; charset=utf-8"

typeJson :: ContentType
typeJson = "application/json; charset=utf-8"

typeXml :: ContentType
typeXml = "text/xml"

typeAtom :: ContentType
typeAtom = "application/atom+xml"

typeRss :: ContentType
typeRss = "application/rss+xml"

typeJpeg :: ContentType
typeJpeg = "image/jpeg"

typePng :: ContentType
typePng = "image/png"

typeGif :: ContentType
typeGif = "image/gif"

typeSvg :: ContentType
typeSvg = "image/svg+xml"

typeJavascript :: ContentType
typeJavascript = "text/javascript; charset=utf-8"

typeCss :: ContentType
typeCss = "text/css; charset=utf-8"

typeFlv :: ContentType
typeFlv = "video/x-flv"

typeOgv :: ContentType
typeOgv = "video/ogg"

typeOctet :: ContentType
typeOctet = "application/octet-stream"

-- | Removes \"extra\" information at the end of a content type string. In
-- particular, removes everything after the semicolon, if present.
--
-- For example, \"text/html; charset=utf-8\" is commonly used to specify the
-- character encoding for HTML data. This function would return \"text/html\".
simpleContentType :: ContentType -> ContentType
simpleContentType = fst . B.break (== _semicolon)

decoderForCharset :: Maybe B.ByteString -> L.ByteString -> TL.Text
decoderForCharset (Just encodingSymbol)
  | encodingSymbol == (encodeUtf8 $ T.pack $ "utf-8")        = LE.decodeUtf8With EE.lenientDecode
  | encodingSymbol == (encodeUtf8 $ T.pack $ "US-ASCII")     = TL.fromStrict . fst . decodeASCIIPrefix . B.toStrict
  | encodingSymbol == (encodeUtf8 $ T.pack $ "latin1")       = LE.decodeLatin1
  | encodingSymbol == (encodeUtf8 $ T.pack $ "GB18030")      = TL.pack . Enc.decodeLazyByteString Enc.GB18030
  | encodingSymbol == (encodeUtf8 $ T.pack $ "windows-1251") = TL.pack . Enc.decodeLazyByteString Enc.CP1251
  | encodingSymbol == (encodeUtf8 $ T.pack $ "Shift_JIS")    = TL.pack . Enc.decodeLazyByteString Enc.ShiftJIS
  | encodingSymbol == (encodeUtf8 $ T.pack $ "Windows-31J")  = TL.pack . Enc.decodeLazyByteString Enc.CP932
  | otherwise = LE.decodeUtf8With EE.lenientDecode
decoderForCharset Nothing = LE.decodeUtf8With EE.lenientDecode

textDecoderFor :: ContentType -> L.ByteString -> Maybe TL.Text
textDecoderFor ct =
  let packString  = (encodeUtf8 . T.pack)
      (t, params) = NWP.parseContentType ct
      charset     = lookup (packString "charset") params
      typeIsText  = B.isPrefixOf (packString "text") t             ||
                    B.isPrefixOf (packString "application/json") t ||
                    B.isPrefixOf (packString "application/rss")  t ||
                    B.isPrefixOf (packString "application/atom") t
      decoder = decoderForCharset charset
  in if isJust charset || typeIsText
     then Just <$> decoder
     else \_ -> Nothing

contentToSnippet :: Content -> (L.ByteString -> Maybe TL.Text) -> I.Int64 -> Maybe TL.Text
contentToSnippet (ContentBuilder builder maybeLength) decoder maxLength = do
  truncatedText <- decoder $ L.take maxLength $ BB.toLazyByteString builder
  pure $ truncatedText <> (TL.pack excessLengthString)
  where
    excessLength = fromMaybe 0 $ (subtract $ fromIntegral maxLength) <$> maybeLength
    excessLengthString = case excessLength > 0 of
      False -> ""
      True -> "...+ " <> (show excessLength)
contentToSnippet (ContentSource _) _ _ = Nothing
contentToSnippet (ContentFile _ _) _ _ = Nothing
contentToSnippet (ContentDontEvaluate _) _ _ = Nothing

typedContentToSnippet :: TypedContent -> I.Int64 -> Maybe TL.Text
typedContentToSnippet (TypedContent t c) maxLength = contentToSnippet c (textDecoderFor t) maxLength

-- | Give just the media types as a pair.
--
-- For example, \"text/html; charset=utf-8\" returns ("text", "html")
contentTypeTypes :: ContentType -> (B.ByteString, B.ByteString)
contentTypeTypes = second tailEmpty . B.break (== _slash) . simpleContentType
  where
    tailEmpty x = if B.null x then "" else B.tail x

instance HasContentType a => HasContentType (DontFullyEvaluate a) where
    getContentType = getContentType . liftM unDontFullyEvaluate

instance ToContent a => ToContent (DontFullyEvaluate a) where
    toContent (DontFullyEvaluate a) = ContentDontEvaluate $ toContent a

instance ToContent J.Value where
    toContent = flip ContentBuilder Nothing
              . J.fromEncoding
              . J.toEncoding

instance ToContent J.Encoding where
    toContent = flip ContentBuilder Nothing . J.fromEncoding

instance HasContentType J.Value where
    getContentType _ = typeJson

instance HasContentType J.Encoding where
    getContentType _ = typeJson

instance HasContentType Html where
    getContentType _ = typeHtml

instance HasContentType Text where
    getContentType _ = typePlain

instance HasContentType T.Text where
    getContentType _ = typePlain

instance HasContentType Css where
    getContentType _ = typeCss

instance HasContentType Javascript where
    getContentType _ = typeJavascript

-- | Any type which can be converted to 'TypedContent'.
--
-- Since 1.2.0
class ToContent a => ToTypedContent a where
    toTypedContent :: a -> TypedContent

instance ToTypedContent TypedContent where
    toTypedContent = id
instance ToTypedContent () where
    toTypedContent () = TypedContent typePlain (toContent ())
instance ToTypedContent Void where
    toTypedContent = absurd
instance ToTypedContent (ContentType, Content) where
    toTypedContent (ct, content) = TypedContent ct content
instance ToTypedContent RepJson where
    toTypedContent (RepJson c) = TypedContent typeJson c
instance ToTypedContent RepPlain where
    toTypedContent (RepPlain c) = TypedContent typePlain c
instance ToTypedContent RepXml where
    toTypedContent (RepXml c) = TypedContent typeXml c
instance ToTypedContent J.Value where
    toTypedContent v = TypedContent typeJson (toContent v)
instance ToTypedContent J.Encoding where
    toTypedContent e = TypedContent typeJson (toContent e)
instance ToTypedContent Html where
    toTypedContent h = TypedContent typeHtml (toContent h)
instance ToTypedContent T.Text where
    toTypedContent t = TypedContent typePlain (toContent t)
instance ToTypedContent [Char] where
    toTypedContent = toTypedContent . pack
instance ToTypedContent Text where
    toTypedContent t = TypedContent typePlain (toContent t)
instance ToTypedContent (JSONResponse a) where
    toTypedContent c = TypedContent typeJson (toContent c)
instance ToTypedContent a => ToTypedContent (DontFullyEvaluate a) where
    toTypedContent (DontFullyEvaluate a) =
        let TypedContent ct c = toTypedContent a
         in TypedContent ct (ContentDontEvaluate c)

instance ToTypedContent Css where
    toTypedContent = TypedContent typeCss . toContent
instance ToTypedContent Javascript where
    toTypedContent = TypedContent typeJavascript . toContent
