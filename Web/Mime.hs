{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Generic MIME type module. Could be spun off into its own package.
module Web.Mime
    ( ContentType (..)
    , typeByExt
    , ext
    ) where

import Data.Function (on)
import Data.Convertible.Text

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

instance Eq ContentType where
    (==) = (==) `on` (cs :: ContentType -> String)

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
