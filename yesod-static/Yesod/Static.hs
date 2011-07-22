{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
---------------------------------------------------------
--
-- Module        : Yesod.Static
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
    , StaticRoute (..)
      -- * Smart constructor
    , static
    , embed
      -- * Template Haskell helpers
    , staticFiles
      -- * Hashing
    , base64md5
    ) where

import Prelude hiding (FilePath)
import qualified Prelude
import System.Directory
--import qualified System.Time
import Control.Monad
import Data.FileEmbed (embedDir)

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
--import Data.IORef (readIORef, newIORef, writeIORef)
import Network.Wai (pathInfo, rawPathInfo, responseLBS)
import Data.Char (isLower, isDigit)
import Data.List (foldl')
import qualified Data.ByteString as S
import Network.HTTP.Types (status301)

import Network.Wai.Application.Static
    ( StaticSettings (..)
    , defaultWebAppSettings
    , fileSystemLookup
    , staticApp
    , embeddedLookup
    , toEmbedded
    , pathFromPieces
    , toFilePath
    )

newtype Static = Static StaticSettings

-- | Default value of 'Static' for a given file folder.
--
-- Does not have index files, uses default directory listings and default mime
-- type list.
static :: Prelude.FilePath -> Static
static fp =
  --hashes <- mkHashMap fp
  Static $ defaultWebAppSettings {
    ssFolder = fileSystemLookup $ toFilePath fp
  }

-- | Produces a 'Static' based on embedding file contents in the executable at
-- compile time.
embed :: Prelude.FilePath -> Q Exp
embed fp =
    [|Static (defaultWebAppSettings
        { ssFolder = embeddedLookup (toEmbedded $(embedDir fp))
        })|]

{-
publicProduction :: String -> Prelude.FilePath -> IO Public
publicProduction root fp = do
  etags <- mkPublicProductionEtag fp
  return $ public root fp etags

publicDevel :: String -> Prelude.FilePath -> IO Public
publicDevel root fp = do
  etags <- mkPublicDevelEtag fp
  return $ public root fp etags
  -}


-- | Manually construct a static route.
-- The first argument is a sub-path to the file being served whereas the second argument is the key value pairs in the query string.
-- For example,
-- > StaticRoute $ StaticR ["thumb001.jpg"] [("foo", "5"), ("bar", "choc")]
-- would generate a url such as 'http://site.com/static/thumb001.jpg?foo=5&bar=choc'
-- The StaticRoute constructor can be used when url's cannot be statically generated at compile-time.
-- E.g. When generating image galleries.
data StaticRoute = StaticRoute [Text] [(Text, Text)]
    deriving (Eq, Show, Read)

type instance Route Static = StaticRoute

instance RenderRoute StaticRoute where
    renderRoute (StaticRoute x y) = (x, y)

instance Yesod master => YesodDispatch Static master where
    -- Need to append trailing slash to make relative links work
    yesodDispatch _ _ [] _ _ = Just $
        \req -> return $ responseLBS status301 [("Location", rawPathInfo req `S.append` "/")] ""

    yesodDispatch (Static set) _ textPieces  _ _ = Just $
        \req -> staticApp set req { pathInfo = textPieces }

notHidden :: Prelude.FilePath -> Bool
notHidden ('.':_) = False
notHidden "tmp" = False
notHidden _ = True

getFileListPieces :: Prelude.FilePath -> IO [[String]]
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
staticFiles :: Prelude.FilePath -> Q [Dec]
staticFiles dir = mkStaticFiles dir

{-
publicFiles :: Prelude.FilePath -> Q [Dec]
publicFiles dir = mkStaticFiles dir PublicSite
-}

mkHashMap :: Prelude.FilePath -> IO (M.Map Prelude.FilePath S8.ByteString)
mkHashMap dir = do
    fs <- getFileListPieces dir
    hashAlist fs >>= return . M.fromList
  where
    hashAlist :: [[String]] -> IO [(Prelude.FilePath, S8.ByteString)]
    hashAlist fs = mapM hashPair fs
      where
        hashPair :: [String] -> IO (Prelude.FilePath, S8.ByteString)
        hashPair pieces = do let file = pathFromRawPieces dir pieces
                             h <- base64md5File file
                             return (file, S8.pack h)

pathFromRawPieces :: Prelude.FilePath -> [String] -> Prelude.FilePath
pathFromRawPieces =
    foldl' append
  where
    append a b = a ++ '/' : b

{-
mkPublicDevelEtag :: Prelude.FilePath -> IO StaticSettings
mkPublicDevelEtag dir = do
    etags <- mkHashMap dir
    mtimeVar <- newIORef (M.empty :: M.Map Prelude.FilePath System.Time.ClockTime)
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


mkPublicProductionEtag :: Prelude.FilePath -> IO StaticSettings
mkPublicProductionEtag dir = do
    etags <- mkHashMap dir
    return $ ETag $ \f -> return . M.lookup f $ etags
-}

data StaticSite = StaticSite | PublicSite
mkStaticFiles :: Prelude.FilePath -> Q [Dec]
mkStaticFiles fp = mkStaticFiles' fp "StaticRoute" True

mkStaticFiles' :: Prelude.FilePath -- ^ static directory
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
        let name' = intercalate "_" $ map (map replace') f
            name = mkName $
                case () of
                    ()
                        | null name' -> error "null-named file"
                        | isDigit (head name') -> '_' : name'
                        | isLower (head name') -> name'
                        | otherwise -> '_' : name'
        f' <- [|map pack $(lift f)|]
        let route = mkName routeConName
        pack' <- [|pack|]
        qs <- if makeHash
                    then do hash <- qRunIO $ base64md5File $ pathFromRawPieces fp f
        -- FIXME hash <- qRunIO . calcHash $ fp ++ '/' : intercalate "/" f
                            [|[(pack $(lift hash), mempty)]|]
                    else return $ ListE []
        return
            [ SigD name $ ConT route
            , FunD name
                [ Clause [] (NormalB $ (ConE route) `AppE` f' `AppE` qs) []
                ]
            ]

base64md5File :: Prelude.FilePath -> IO String
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
        handler = fromMaybe notFound $ handleSite staticSite (error "Yesod.Static: getSTaticHandler") route "GET"
-}


{-
calcHash :: Prelude.FilePath -> IO String
calcHash fname =
    withBinaryFile fname ReadMode hashHandle
  where
    hashHandle h = do s <- L.hGetContents h
                      return $! base64md5 s
                      -}
