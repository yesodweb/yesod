{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
---------------------------------------------------------
--
-- Module        : Web.Restful.Helpers.Static
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Serve static files from a Restful app.
--
---------------------------------------------------------
module Web.Restful.Helpers.Static
    ( serveStatic
    , FileLookup
    ) where

import qualified Data.ByteString as B

import Web.Restful

type FileLookup = FilePath -> IO (Maybe B.ByteString)

serveStatic :: FileLookup -> Verb -> Handler
serveStatic fl Get = getStatic fl
serveStatic _ _ = notFound

newtype StaticReq = StaticReq FilePath
instance Request StaticReq where
    parseRequest = StaticReq `fmap` urlParam "filepath" -- FIXME check for ..

getStatic :: FileLookup -> Handler
getStatic fl = do
    StaticReq fp <- getRequest
    content <- liftIO $ fl fp
    case content of
        Nothing -> notFound
        Just bs -> genResponse (mimeType $ ext fp) bs

mimeType :: String -> String
mimeType "jpg" = "image/jpeg"
mimeType "jpeg" = "image/jpeg"
mimeType "js" = "text/javascript"
mimeType "css" = "text/css"
mimeType "html" = "text/html"
mimeType "png" = "image/png"
mimeType "gif" = "image/gif"
mimeType "txt" = "text/plain"
mimeType _ = "application/octet-stream"

ext :: String -> String
ext = reverse . fst . break (== '.') . reverse
