{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-} -- FIXME due to bug in web-routes-quasi
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
    ( FileLookup
    , fileLookupDir
    , siteStatic
    , StaticRoutes
    , toStaticRoute
    , staticArgs
    , Static
    ) where

import System.Directory (doesFileExist)
import Control.Monad

import Yesod
import Data.List (intercalate)
import Network.Wai

type FileLookup = FilePath -> IO (Maybe (Either FilePath Content))

data Static = Static FileLookup

staticArgs :: FileLookup -> Static
staticArgs = Static

-- FIXME bug in web-routes-quasi generates warning here
$(mkYesodSub "Static" [] [$parseRoutes|
/* StaticRoute GET
|])

-- | A 'FileLookup' for files in a directory. Note that this function does not
-- check if the requested path does unsafe things, eg expose hidden files. You
-- should provide this checking elsewhere.
--
-- If you are just using this in combination with serveStatic, serveStatic
-- provides this checking.
fileLookupDir :: FilePath -> Static
fileLookupDir dir = Static $ \fp -> do
    let fp' = dir ++ '/' : fp
    exists <- doesFileExist fp'
    if exists
        then return $ Just $ Left fp'
        else return Nothing

getStatic :: FileLookup -> [String] -> GHandler sub master [(ContentType, Content)]
getStatic fl fp' = do
    when (any isUnsafe fp') notFound
    wai <- waiRequest
    when (requestMethod wai /= GET) badMethod
    let fp = intercalate "/" fp'
    content <- liftIO $ fl fp
    case content of
        Nothing -> notFound
        Just (Left fp'') -> sendFile (typeByExt $ ext fp'') fp''
        Just (Right bs) -> return [(typeByExt $ ext fp, cs bs)]
  where
    isUnsafe [] = True
    isUnsafe ('.':_) = True
    isUnsafe _ = False

getStaticRoute :: [String] -> GHandler Static master [(ContentType, Content)]
getStaticRoute fp = do
    Static fl <- getYesod
    getStatic fl fp

toStaticRoute :: [String] -> StaticRoutes
toStaticRoute = StaticRoute
