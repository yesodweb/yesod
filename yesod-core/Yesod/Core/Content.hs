{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
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

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy (Text, pack)
import qualified Data.Text as T
import Control.Monad (liftM)

import Blaze.ByteString.Builder (Builder, fromByteString, fromLazyByteString)
import Data.Monoid (mempty)

import Text.Hamlet (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtmlBuilder)
import Data.Conduit (Source, Flush (Chunk), ResumableSource, mapOutput)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit.Internal (ResumableSource (ResumableSource))
import qualified Data.Conduit.Internal as CI

import qualified Data.Aeson as J
#if MIN_VERSION_aeson(0, 7, 0)
import Data.Aeson.Encode (encodeToTextBuilder)
#else
import Data.Aeson.Encode (fromValue)
#endif
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import Data.Text.Lazy.Builder (toLazyText)
import Yesod.Core.Types
import Text.Lucius (Css, renderCss)
import Text.Julius (Javascript, unJavascript)

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
    toContent bs = ContentBuilder (fromByteString bs) $ Just $ B.length bs
instance ToContent L.ByteString where
    toContent = flip ContentBuilder Nothing . fromLazyByteString
instance ToContent T.Text where
    toContent = toContent . Blaze.fromText
instance ToContent Text where
    toContent = toContent . Blaze.fromLazyText
instance ToContent String where
    toContent = toContent . Blaze.fromString
instance ToContent Html where
    toContent bs = ContentBuilder (renderHtmlBuilder bs) Nothing
instance ToContent () where
    toContent () = toContent B.empty
instance ToContent (ContentType, Content) where
    toContent = snd
instance ToContent TypedContent where
    toContent (TypedContent _ c) = c

instance ToContent Css where
    toContent = toContent . renderCss
instance ToContent Javascript where
    toContent = toContent . toLazyText . unJavascript

instance ToFlushBuilder builder => ToContent (CI.Pipe () () builder () (ResourceT IO) ()) where
    toContent src = ContentSource $ CI.ConduitM (CI.mapOutput toFlushBuilder src >>=)

instance ToFlushBuilder builder => ToContent (Source (ResourceT IO) builder) where
    toContent src = ContentSource $ mapOutput toFlushBuilder src
instance ToFlushBuilder builder => ToContent (ResumableSource (ResourceT IO) builder) where
    toContent (ResumableSource src _) = toContent src

-- | A class for all data which can be sent in a streaming response. Note that
-- for textual data, instances must use UTF-8 encoding.
--
-- Since 1.2.0
class ToFlushBuilder a where toFlushBuilder :: a -> Flush Builder
instance ToFlushBuilder (Flush Builder) where toFlushBuilder = id
instance ToFlushBuilder Builder where toFlushBuilder = Chunk
instance ToFlushBuilder (Flush B.ByteString) where toFlushBuilder = fmap fromByteString
instance ToFlushBuilder B.ByteString where toFlushBuilder = Chunk . fromByteString
instance ToFlushBuilder (Flush L.ByteString) where toFlushBuilder = fmap fromLazyByteString
instance ToFlushBuilder L.ByteString where toFlushBuilder = Chunk . fromLazyByteString
instance ToFlushBuilder (Flush Text) where toFlushBuilder = fmap Blaze.fromLazyText
instance ToFlushBuilder Text where toFlushBuilder = Chunk . Blaze.fromLazyText
instance ToFlushBuilder (Flush T.Text) where toFlushBuilder = fmap Blaze.fromText
instance ToFlushBuilder T.Text where toFlushBuilder = Chunk . Blaze.fromText
instance ToFlushBuilder (Flush String) where toFlushBuilder = fmap Blaze.fromString
instance ToFlushBuilder String where toFlushBuilder = Chunk . Blaze.fromString
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
simpleContentType = fst . B.breakByte 59 -- 59 == ;

-- Give just the media types as a pair.
-- For example, \"text/html; charset=utf-8\" returns ("text", "html")
contentTypeTypes :: ContentType -> (B.ByteString, B.ByteString)
contentTypeTypes ct = (main, fst $ B.breakByte semicolon (tailEmpty sub))
  where
    tailEmpty x = if B.null x then "" else B.tail x
    (main, sub) = B.breakByte slash ct
    slash = 47
    semicolon = 59


instance HasContentType a => HasContentType (DontFullyEvaluate a) where
    getContentType = getContentType . liftM unDontFullyEvaluate

instance ToContent a => ToContent (DontFullyEvaluate a) where
    toContent (DontFullyEvaluate a) = ContentDontEvaluate $ toContent a

instance ToContent J.Value where
    toContent = flip ContentBuilder Nothing
              . Blaze.fromLazyText
              . toLazyText
#if MIN_VERSION_aeson(0, 7, 0)
              . encodeToTextBuilder
#else
              . fromValue
#endif
instance HasContentType J.Value where
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
instance ToTypedContent Html where
    toTypedContent h = TypedContent typeHtml (toContent h)
instance ToTypedContent T.Text where
    toTypedContent t = TypedContent typePlain (toContent t)
instance ToTypedContent [Char] where
    toTypedContent = toTypedContent . pack
instance ToTypedContent Text where
    toTypedContent t = TypedContent typePlain (toContent t)
instance ToTypedContent a => ToTypedContent (DontFullyEvaluate a) where
    toTypedContent (DontFullyEvaluate a) =
        let TypedContent ct c = toTypedContent a
         in TypedContent ct (ContentDontEvaluate c)

instance ToTypedContent Css where
    toTypedContent = TypedContent typeCss . toContent
instance ToTypedContent Javascript where
    toTypedContent = TypedContent typeJavascript . toContent
