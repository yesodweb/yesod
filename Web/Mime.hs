{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
-- | Generic MIME type module. Could be spun off into its own package.
module Web.Mime
    ( -- * Data type and conversions
      ContentType (..)
    , contentTypeFromString
    , contentTypeToString
      -- * File extensions
    , typeByExt
    , ext
      -- * Utilities
    , simpleContentType
#if TEST
    , testSuite
#endif
    ) where

import Data.Function (on)

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck
import Control.Monad (when)
#endif

-- | Equality is determined by converting to a 'String' via
-- 'contentTypeToString'. This ensures that, for example, 'TypeJpeg' is the
-- same as 'TypeOther' \"image/jpeg\". However, note that 'TypeHtml' is *not*
-- the same as 'TypeOther' \"text/html\", since 'TypeHtml' is defined as UTF-8
-- encoded. See 'contentTypeToString'.
data ContentType =
    TypeHtml
    | TypePlain
    | TypeJson
    | TypeXml
    | TypeAtom
    | TypeJpeg
    | TypePng
    | TypeGif
    | TypeJavascript
    | TypeCss
    | TypeFlv
    | TypeOgv
    | TypeOctet
    | TypeOther String
    deriving (Show)

-- | This is simply a synonym for 'TypeOther'. However, equality works as
-- expected; see 'ContentType'.
contentTypeFromString :: String -> ContentType
contentTypeFromString = TypeOther

-- | This works as expected, with one caveat: the builtin textual content types
-- ('TypeHtml', 'TypePlain', etc) all include \"; charset=utf-8\" at the end of
-- their basic content-type. If another encoding is desired, please use
-- 'TypeOther'.
contentTypeToString :: ContentType -> String
contentTypeToString TypeHtml = "text/html; charset=utf-8"
contentTypeToString TypePlain = "text/plain; charset=utf-8"
contentTypeToString TypeJson = "application/json; charset=utf-8"
contentTypeToString TypeXml = "text/xml"
contentTypeToString TypeAtom = "application/atom+xml"
contentTypeToString TypeJpeg = "image/jpeg"
contentTypeToString TypePng = "image/png"
contentTypeToString TypeGif = "image/gif"
contentTypeToString TypeJavascript = "text/javascript; charset=utf-8"
contentTypeToString TypeCss = "text/css; charset=utf-8"
contentTypeToString TypeFlv = "video/x-flv"
contentTypeToString TypeOgv = "video/ogg"
contentTypeToString TypeOctet = "application/octet-stream"
contentTypeToString (TypeOther s) = s

-- | Removes \"extra\" information at the end of a content type string. In
-- particular, removes everything after the semicolon, if present.
--
-- For example, \"text/html; charset=utf-8\" is commonly used to specify the
-- character encoding for HTML data. This function would return \"text/html\".
simpleContentType :: String -> String
simpleContentType = fst . span (/= ';')

instance Eq ContentType where
    (==) = (==) `on` contentTypeToString

-- | Determine a mime-type based on the file extension.
typeByExt :: String -> ContentType
typeByExt "jpg" = TypeJpeg
typeByExt "jpeg" = TypeJpeg
typeByExt "js" = TypeJavascript
typeByExt "css" = TypeCss
typeByExt "html" = TypeHtml
typeByExt "png" = TypePng
typeByExt "gif" = TypeGif
typeByExt "txt" = TypePlain
typeByExt "flv" = TypeFlv
typeByExt "ogv" = TypeOgv
typeByExt _ = TypeOctet

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
    TypeJavascript @=? typeByExt (ext "foo.js")
    TypeHtml @=? typeByExt (ext "foo.html")
#endif
