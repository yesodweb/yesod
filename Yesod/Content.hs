{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}

module Yesod.Content
    ( -- * Content
      Content
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
    , typeJpeg
    , typePng
    , typeGif
    , typeJavascript
    , typeCss
    , typeFlv
    , typeOgv
    , typeOctet
      -- ** File extensions
    , typeByExt
    , ext
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
    , formatCookieExpires
#if TEST
    , testSuite
#endif
    ) where

import Data.Maybe (mapMaybe)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Text.Lazy (Text)
import qualified Data.Text as T

import qualified Network.Wai as W

import Data.Time
import System.Locale

import qualified Data.Text.Encoding
import qualified Data.Text.Lazy.Encoding

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
#endif

type Content = W.ResponseBody

-- | Zero-length enumerator.
emptyContent :: Content
emptyContent = W.ResponseLBS L.empty

-- | Anything which can be converted into 'Content'. Most of the time, you will
-- want to use the 'ContentEnum' constructor. An easier approach will be to use
-- a pre-defined 'toContent' function, such as converting your data into a lazy
-- bytestring and then calling 'toContent' on that.
class ToContent a where
    toContent :: a -> Content

instance ToContent B.ByteString where
    toContent = W.ResponseLBS . L.fromChunks . return
instance ToContent L.ByteString where
    toContent = W.ResponseLBS
instance ToContent T.Text where
    toContent = toContent . Data.Text.Encoding.encodeUtf8
instance ToContent Text where
    toContent = W.ResponseLBS . Data.Text.Lazy.Encoding.encodeUtf8
instance ToContent String where
    toContent = toContent . T.pack

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
    chooseRep = defChooseRep [(typePlain, const $ return $ toContent "")]

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

type ContentType = String

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
simpleContentType :: String -> String
simpleContentType = fst . span (/= ';')

-- | A default extension to mime-type dictionary.
typeByExt :: [(String, ContentType)]
typeByExt =
    [ ("jpg", typeJpeg)
    , ("jpeg", typeJpeg)
    , ("js", typeJavascript)
    , ("css", typeCss)
    , ("html", typeHtml)
    , ("png", typePng)
    , ("gif", typeGif)
    , ("txt", typePlain)
    , ("flv", typeFlv)
    , ("ogv", typeOgv)
    ]

-- | Get a file extension (everything after last period).
ext :: String -> String
ext = reverse . fst . break (== '.') . reverse

#if TEST
---- Testing
testSuite :: Test
testSuite = testGroup "Yesod.Resource"
    [ testProperty "ext" propExt
    , testCase "typeByExt" caseTypeByExt
    ]

propExt :: String -> Bool
propExt s =
    let s' = filter (/= '.') s
     in s' == ext ("foobarbaz." ++ s')

caseTypeByExt :: Assertion
caseTypeByExt = do
    Just typeJavascript @=? lookup (ext "foo.js") typeByExt
    Just typeHtml @=? lookup (ext "foo.html") typeByExt
#endif

-- | Format a 'UTCTime' in W3 format.
formatW3 :: UTCTime -> String
formatW3 = formatTime defaultTimeLocale "%FT%X-00:00"

-- | Format as per RFC 1123.
formatRFC1123 :: UTCTime -> String
formatRFC1123 = formatTime defaultTimeLocale "%a, %d %b %Y %X %Z"

-- | Format a 'UTCTime' for a cookie.
formatCookieExpires :: UTCTime -> String
formatCookieExpires = formatTime defaultTimeLocale "%a, %d-%b-%Y %X GMT"
