{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Content
    ( -- * Content
      Content (..)
    , emptyContent
    , ToContent (..)
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
    , typeJavascript
    , typeCss
    , typeFlv
    , typeOgv
    , typeOctet
      -- * Utilities
    , simpleContentType
      -- * Representations
    , ChooseRep
    , HasReps (..)
    , defChooseRep
      -- ** Specific content types
    , RepHtml (..)
    , RepJson (..)
    , RepHtmlJson (..)
    , RepPlain (..)
    , RepXml (..)
      -- * Utilities
    , formatW3
    , formatRFC1123
    , formatRFC822
    ) where

import Data.Maybe (mapMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy (Text, pack)
import qualified Data.Text as T

import Data.Time
import System.Locale

import qualified Data.Text.Encoding
import qualified Data.Text.Lazy.Encoding

import Data.Enumerator (Enumerator)
import Blaze.ByteString.Builder (Builder, fromByteString, fromLazyByteString)
import Data.Monoid (mempty)

import Text.Hamlet (Html)
import Text.Blaze.Renderer.Utf8 (renderHtmlBuilder)
import Data.String (IsString (fromString))

data Content = ContentBuilder Builder (Maybe Int) -- ^ The content and optional content length.
             | ContentEnum (forall a. Enumerator Builder IO a)
             | ContentFile FilePath

-- | Zero-length enumerator.
emptyContent :: Content
emptyContent = ContentBuilder mempty $ Just 0

instance IsString Content where
    fromString = toContent

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

instance ToContent Builder where
    toContent = flip ContentBuilder Nothing
instance ToContent B.ByteString where
    toContent bs = ContentBuilder (fromByteString bs) $ Just $ B.length bs
instance ToContent L.ByteString where
    toContent = flip ContentBuilder Nothing . fromLazyByteString
instance ToContent T.Text where
    toContent = toContent . Data.Text.Encoding.encodeUtf8
instance ToContent Text where
    toContent = toContent . Data.Text.Lazy.Encoding.encodeUtf8
instance ToContent String where
    toContent = toContent . pack
instance ToContent Html where
    toContent bs = ContentBuilder (renderHtmlBuilder bs) Nothing

-- | A function which gives targetted representations of content based on the
-- content-types the user accepts.
type ChooseRep =
    [ContentType] -- ^ list of content-types user accepts, ordered by preference
 -> IO (ContentType, Content)

-- | Any type which can be converted to representations.
class HasReps a where
    chooseRep :: a -> ChooseRep

-- | A helper method for generating 'HasReps' instances.
--
-- This function should be given a list of pairs of content type and conversion
-- functions. If none of the content types match, the first pair is used.
defChooseRep :: [(ContentType, a -> IO Content)] -> a -> ChooseRep
defChooseRep reps a ts = do
  let (ct, c) =
        case mapMaybe helper ts of
            (x:_) -> x
            [] -> case reps of
                    [] -> error "Empty reps to defChooseRep"
                    (x:_) -> x
  c' <- c a
  return (ct, c')
        where
            helper ct = do
                c <- lookup ct reps
                return (ct, c)

instance HasReps ChooseRep where
    chooseRep = id

instance HasReps () where
    chooseRep = defChooseRep [(typePlain, const $ return $ toContent B.empty)]

instance HasReps (ContentType, Content) where
    chooseRep = const . return

instance HasReps [(ContentType, Content)] where
    chooseRep a cts = return $
        case filter (\(ct, _) -> go ct `elem` map go cts) a of
            ((ct, c):_) -> (ct, c)
            _ -> case a of
                    (x:_) -> x
                    _ -> error "chooseRep [(ContentType, Content)] of empty"
      where
        go = simpleContentType

newtype RepHtml = RepHtml Content
instance HasReps RepHtml where
    chooseRep (RepHtml c) _ = return (typeHtml, c)
newtype RepJson = RepJson Content
instance HasReps RepJson where
    chooseRep (RepJson c) _ = return (typeJson, c)
data RepHtmlJson = RepHtmlJson Content Content
instance HasReps RepHtmlJson where
    chooseRep (RepHtmlJson html json) = chooseRep
        [ (typeHtml, html)
        , (typeJson, json)
        ]
newtype RepPlain = RepPlain Content
instance HasReps RepPlain where
    chooseRep (RepPlain c) _ = return (typePlain, c)
newtype RepXml = RepXml Content
instance HasReps RepXml where
    chooseRep (RepXml c) _ = return (typeXml, c)

type ContentType = B.ByteString

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
simpleContentType :: B.ByteString -> B.ByteString
simpleContentType = S8.takeWhile (/= ';')

-- | Format a 'UTCTime' in W3 format.
formatW3 :: UTCTime -> String
formatW3 = formatTime defaultTimeLocale "%FT%X-00:00"

-- | Format as per RFC 1123.
formatRFC1123 :: UTCTime -> String
formatRFC1123 = formatTime defaultTimeLocale "%a, %d %b %Y %X %Z"

-- | Format as per RFC 822.
formatRFC822 :: UTCTime -> String
formatRFC822 = formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S %z"
