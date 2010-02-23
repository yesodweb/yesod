{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
-- | Generic MIME type module. Could be spun off into its own package.
module Web.Mime
    ( ContentType (..)
    , contentTypeFromBS
    , typeByExt
    , ext
    , simpleContentType
#if TEST
    , testSuite
#endif
    ) where

import Data.Function (on)
import Data.Convertible.Text
import Data.ByteString.Char8 (pack, ByteString, unpack)

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck
import Control.Monad (when)
#endif

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

instance ConvertSuccess ContentType ByteString where
    convertSuccess = pack . cs

instance ConvertSuccess ContentType [Char] where
    convertSuccess TypeHtml = "text/html; charset=utf-8"
    convertSuccess TypePlain = "text/plain; charset=utf-8"
    convertSuccess TypeJson = "application/json; charset=utf-8"
    convertSuccess TypeXml = "text/xml"
    convertSuccess TypeAtom = "application/atom+xml"
    convertSuccess TypeJpeg = "image/jpeg"
    convertSuccess TypePng = "image/png"
    convertSuccess TypeGif = "image/gif"
    convertSuccess TypeJavascript = "text/javascript; charset=utf-8"
    convertSuccess TypeCss = "text/css; charset=utf-8"
    convertSuccess TypeFlv = "video/x-flv"
    convertSuccess TypeOgv = "video/ogg"
    convertSuccess TypeOctet = "application/octet-stream"
    convertSuccess (TypeOther s) = s

simpleContentType :: ContentType -> String
simpleContentType = fst . span (/= ';') . cs

instance Eq ContentType where
    (==) = (==) `on` (cs :: ContentType -> String)

contentTypeFromBS :: ByteString -> ContentType
contentTypeFromBS = TypeOther . unpack

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
