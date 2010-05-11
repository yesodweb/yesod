{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
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

-- | Serve static files from a Yesod app.
--
-- This is most useful for standalone testing. When running on a production
-- server (like Apache), just let the server do the static serving.
--
-- In fact, in an ideal setup you'll serve your static files from a separate
-- domain name to save time on transmitting cookies. In that case, you may wish
-- to use 'urlRenderOverride' to redirect requests to this subsite to a
-- separate domain name.
module Yesod.Helpers.Static
    ( -- * Subsite
      Static (..)
    , StaticRoutes (..)
    , siteStatic
      -- * Lookup files in filesystem
    , fileLookupDir
    ) where

import System.Directory (doesFileExist)
import Control.Monad

import Yesod
import Data.List (intercalate)

-- | A function for looking up file contents. For serving from the file system,
-- see 'fileLookupDir'.
data Static = Static (FilePath -> IO (Maybe (Either FilePath Content)))

$(mkYesodSub "Static" [] [$parseRoutes|
/* StaticRoute GET
|])

-- | Lookup files in a specific directory.
--
-- If you are just using this in combination with the static subsite (you
-- probably are), the handler itself checks that no unsafe paths are being
-- requested. In particular, no path segments may begin with a single period,
-- so hidden files and parent directories are safe.
fileLookupDir :: FilePath -> Static
fileLookupDir dir = Static $ \fp -> do
    let fp' = dir ++ '/' : fp
    exists <- doesFileExist fp'
    if exists
        then return $ Just $ Left fp'
        else return Nothing

getStaticRoute :: [String]
               -> GHandler Static master [(ContentType, Content)]
getStaticRoute fp' = do
    Static fl <- getYesodSub
    when (any isUnsafe fp') notFound
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
