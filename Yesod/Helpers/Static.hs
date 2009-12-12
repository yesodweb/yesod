{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
---------------------------------------------------------
--
-- Module        : Yesod.Helpers.Static
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Serve static files from a Yesod app.
--
-- This is most useful for standalone testing. When running on a production
-- server (like Apache), just let the server do the static serving.
--
---------------------------------------------------------
module Yesod.Helpers.Static
    ( serveStatic
    , FileLookup
    , fileLookupDir
    ) where

import qualified Data.ByteString.Lazy as B
import System.Directory (doesFileExist)
import Control.Applicative ((<$>))

import Yesod
import Yesod.Rep

type FileLookup = FilePath -> IO (Maybe B.ByteString)

-- | A 'FileLookup' for files in a directory.
fileLookupDir :: FilePath -> FileLookup
fileLookupDir dir fp = do
    let fp' = dir ++ '/' : fp -- FIXME incredibly insecure...
    exists <- doesFileExist fp'
    if exists
        then Just <$> B.readFile fp'
        else return Nothing

serveStatic :: FileLookup -> Verb -> Handler [(ContentType, Content)]
serveStatic fl Get = getStatic fl
serveStatic _ _ = notFound

getStatic :: FileLookup -> Handler [(ContentType, Content)]
getStatic fl = do
    fp <- urlParam "filepath" -- FIXME check for ..
    content <- liftIO $ fl fp
    case content of
        Nothing -> notFound
        Just bs -> return [(mimeType $ ext fp, Content bs)]

mimeType :: String -> ContentType
mimeType "jpg" = TypeJpeg
mimeType "jpeg" = TypeJpeg
mimeType "js" = TypeJavascript
mimeType "css" = TypeCss
mimeType "html" = TypeHtml
mimeType "png" = TypePng
mimeType "gif" = TypeGif
mimeType "txt" = TypePlain
mimeType "flv" = TypeFlv
mimeType "ogv" = TypeOgv
mimeType _ = TypeOctet

ext :: String -> String
ext = reverse . fst . break (== '.') . reverse
