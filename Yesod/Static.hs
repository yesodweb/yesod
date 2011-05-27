{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
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

-- | Serve static files from a Yesod app.
--
-- This is most useful for standalone testing. When running on a production
-- server (like Apache), just let the server do the static serving.
--
-- In fact, in an ideal setup you'll serve your static files from a separate
-- domain name to save time on transmitting cookies. In that case, you may wish
-- to use 'urlRenderOverride' to redirect requests to this subsite to a
-- separate domain name.
module Yesod.Static
    ( -- * Subsite
      Static (..)
    , Public (..)
    , StaticRoute (..)
    , PublicRoute (..)
      -- * Smart constructor
    , static
    , publicProduction
    , publicDevel
      -- * Template Haskell helpers
    , staticFiles
    , publicFiles
    {-
      -- * Embed files
    , getStaticHandler
    -}
      -- * Hashing
    , base64md5
#if TEST
    , getFileListPieces 
#endif
    ) where

import System.Directory
import qualified System.Time
import Control.Monad

import Yesod.Handler
import Yesod.Core

import Data.List (intercalate)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Char8 as S8
import qualified Data.Serialize
import Data.Text (Text, pack)
import Data.Monoid (mempty)
import qualified Data.Map as M
import Data.IORef (readIORef, newIORef, writeIORef)

import Network.Wai.Application.Static
    ( StaticSettings (..), CacheSettings (..)
    , defaultStaticSettings, defaultPublicSettings
    , staticAppPieces
    , pathFromPieces
    )

-- | generally static assets referenced in html files
-- assets get a checksum query parameter appended for perfect caching
-- * a far future expire date is set
-- * a given asset revision will only ever be downloaded once (if the browser maintains its cache)
-- if you don't want to see a checksum in the url- use Public
newtype Static = Static StaticSettings
-- | same as Static, but there is no checksum query parameter appended
-- generally html files and the favicon, but could be any file where you don't want the checksum parameter
-- * the file checksum is used for an ETag.
-- * this form of caching is not as good as the static- the browser can avoid downloading the file, but it always need to send a request with the etag value to the server to see if its copy is up to date
newtype Public = Public StaticSettings

-- | Default value of 'Static' for a given file folder.
--
-- Does not have index files, uses default directory listings and default mime
-- type list.
static :: String -> FilePath -> IO Static
static root fp = do
  hashes <- mkHashMap fp
  return $ Static $ (defaultStaticSettings (Forever $ isStaticRequest hashes)) {
    ssFolder = fp
  , ssMkRedirect = \_ newPath -> S8.append (S8.pack (root ++ "/")) newPath
  }
  where
    isStaticRequest hashes reqf reqh = case M.lookup reqf hashes of
                               Nothing -> False
                               Just h  -> h == reqh

-- | no directory listing
public :: String -> FilePath -> CacheSettings -> Public
public root fp cache = Public $ (defaultPublicSettings cache) {
    ssFolder = fp 
  , ssMkRedirect = \_ newPath -> S8.append (S8.pack (root ++ "/")) newPath
  }

publicProduction :: String -> FilePath -> IO Public
publicProduction root fp = do
  etags <- mkPublicProductionEtag fp
  return $ public root fp etags

publicDevel :: String -> FilePath -> IO Public
publicDevel root fp = do
  etags <- mkPublicDevelEtag fp
  return $ public root fp etags


-- | Manually construct a static route.
-- The first argument is a sub-path to the file being served whereas the second argument is the key value pairs in the query string.
-- For example,
-- > StaticRoute $ StaticR ["thumb001.jpg"] [("foo", "5"), ("bar", "choc")]
-- would generate a url such as 'http://site.com/static/thumb001.jpg?foo=5&bar=choc'
-- The StaticRoute constructor can be used when url's cannot be statically generated at compile-time.
-- E.g. When generating image galleries.
data StaticRoute = StaticRoute [Text] [(Text, Text)]
    deriving (Eq, Show, Read)
data PublicRoute = PublicRoute [Text] [(Text, Text)]
    deriving (Eq, Show, Read)

type instance Route Static = StaticRoute
type instance Route Public = PublicRoute

instance RenderRoute StaticRoute where
    renderRoute (StaticRoute x y) = (x, y)
instance RenderRoute PublicRoute where
    renderRoute (PublicRoute x y) = (x, y)

instance Yesod master => YesodDispatch Static master where
    yesodDispatch (Static set) _ pieces  _ _ =
        Just $ staticAppPieces set pieces

instance Yesod master => YesodDispatch Public master where
    yesodDispatch (Public set) _ pieces  _ _ =
        Just $ staticAppPieces set pieces

notHidden :: FilePath -> Bool
notHidden ('.':_) = False
notHidden "tmp" = False
notHidden _ = True

getFileListPieces :: FilePath -> IO [[String]]
getFileListPieces = flip go id
  where
    go :: String -> ([String] -> [String]) -> IO [[String]]
    go fp front = do
        allContents <- filter notHidden `fmap` getDirectoryContents fp
        let fullPath :: String -> String
            fullPath f = fp ++ '/' : f
        files <- filterM (doesFileExist . fullPath) allContents
        let files' = map (front . return) files
        dirs <- filterM (doesDirectoryExist . fullPath) allContents
        dirs' <- mapM (\f -> go (fullPath f) (front . (:) f)) dirs
        return $ concat $ files' : dirs'

-- | This piece of Template Haskell will find all of the files in the given directory and create Haskell identifiers for them. For example, if you have the files \"static\/style.css\" and \"static\/js\/script.js\", it will essentailly create:
--
-- > style_css = StaticRoute ["style.css"] []
-- > js_script_js = StaticRoute ["js/script.js"] []
staticFiles :: FilePath -> Q [Dec]
staticFiles dir = mkStaticFiles dir StaticSite

publicFiles :: FilePath -> Q [Dec]
publicFiles dir = mkStaticFiles dir PublicSite

mkHashMap :: FilePath -> IO (M.Map FilePath S8.ByteString)
mkHashMap dir = do
    fs <- getFileListPieces dir
    hashAlist fs >>= return . M.fromList
  where
    hashAlist :: [[String]] -> IO [(FilePath, S8.ByteString)]
    hashAlist fs = mapM hashPair fs
      where
        hashPair :: [String] -> IO (FilePath, S8.ByteString)
        hashPair pieces = do let file = pathFromPieces dir (map pack pieces)
                             h <- base64md5File file
                             return (file, S8.pack h)

mkPublicDevelEtag :: FilePath -> IO CacheSettings
mkPublicDevelEtag dir = do
    etags <- mkHashMap dir
    mtimeVar <- newIORef (M.empty :: M.Map FilePath System.Time.ClockTime)
    return $ ETag $ \f ->
      case M.lookup f etags of
        Nothing -> return Nothing
        Just checksum -> do
          newt <- getModificationTime f
          mtimes <- readIORef mtimeVar
          oldt <- case M.lookup f mtimes of
            Nothing -> writeIORef mtimeVar (M.insert f newt mtimes) >> return newt
            Just ot -> return ot
          return $ if newt /= oldt then Nothing else Just checksum


mkPublicProductionEtag :: FilePath -> IO CacheSettings
mkPublicProductionEtag dir = do
    etags <- mkHashMap dir
    return $ ETag $ \f -> return . M.lookup f $ etags

data StaticSite = StaticSite | PublicSite
mkStaticFiles :: FilePath -> StaticSite -> Q [Dec]
mkStaticFiles fp StaticSite = mkStaticFiles' fp "StaticRoute" True
mkStaticFiles fp PublicSite = mkStaticFiles' fp "PublicRoute" False

mkStaticFiles' :: FilePath -- ^ static directory
               -> String   -- ^ route constructor "StaticRoute"
               -> Bool     -- ^ append checksum query parameter
               -> Q [Dec]
mkStaticFiles' fp routeConName makeHash = do
    fs <- qRunIO $ getFileListPieces fp
    concat `fmap` mapM mkRoute fs
  where
    replace' c
        | 'A' <= c && c <= 'Z' = c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
        | otherwise = '_'
    mkRoute f = do
        let name = mkName $ intercalate "_" $ map (map replace') f
        f' <- [|map pack $(lift f)|]
        let route = mkName routeConName
        pack' <- [|pack|]
        qs <- if makeHash
                    then do hash <- qRunIO $ base64md5File $ pathFromPieces fp (map pack f)
                            [|[(pack $(lift hash), mempty)]|]
                    else return $ ListE []
        return
            [ SigD name $ ConT route
            , FunD name
                [ Clause [] (NormalB $ (ConE route) `AppE` f' `AppE` qs) []
                ]
            ]

base64md5File :: FilePath -> IO String
base64md5File file = do
  contents <- L.readFile file
  return $ base64md5 contents

-- | md5-hashes the given lazy bytestring and returns the hash as
-- base64url-encoded string.
--
-- This function returns the first 8 characters of the hash.
base64md5 :: L.ByteString -> String
base64md5 = map tr
          . take 8
          . S8.unpack
          . Data.ByteString.Base64.encode
          . Data.Serialize.encode
          . md5
  where
    tr '+' = '-'
    tr '/' = '_'
    tr c   = c

{- FIXME
-- | Dispatch static route for a subsite
--
-- Subsites with static routes can't (yet) define Static routes the same way "master" sites can.
-- Instead of a subsite route:
-- /static StaticR Static getStatic
-- Use a normal route:
-- /static/*Strings StaticR GET
--
-- Then, define getStaticR something like:
-- getStaticR = getStaticHandler ($(mkEmbedFiles "static") typeByExt) StaticR
-- */ end CPP comment
getStaticHandler :: Static -> (StaticRoute -> Route sub) -> [String] -> GHandler sub y ChooseRep
getStaticHandler static toSubR pieces = do
  toMasterR <- getRouteToMaster   
  toMasterHandler (toMasterR . toSubR) toSub route handler
  where route = StaticRoute pieces []
        toSub _ = static
        staticSite = getSubSite :: Site (Route Static) (String -> Maybe (GHandler Static y ChooseRep))
        handler = fromMaybe notFound $ handleSite staticSite undefined route "GET"
-}

