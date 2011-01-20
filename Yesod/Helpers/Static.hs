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
module Yesod.Helpers.Static
    ( -- * Subsite
      Static (..)
    , StaticRoute (..)
      -- * Lookup files in filesystem
    , staticFiles
      -- * Embed files
    , getStaticHandler
      -- * Hashing
    , base64md5
#if TEST
    , testSuite
#endif
    ) where

import System.Directory
import Control.Monad
import Data.Maybe (fromMaybe)

import Yesod.Handler
import Yesod.Content
import Yesod.Core
import Yesod.Request

import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.Class as Trans

import Data.List (intercalate)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Web.Routes

import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Char8 as S8
import qualified Data.Serialize

import Network.Wai.Application.Static
    (defaultMimeTypeByExt, StaticSettings (..), staticApp, defaultListing)

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
#endif

-- | A function for looking up file contents. For serving from the file system,
-- see 'fileLookupDir'.
data Static = Static
    { staticPrefix :: FilePath
    -- FIXME why not just put in a StaticSettings here?
    }

-- | Manually construct a static route.
-- The first argument is a sub-path to the file being served whereas the second argument is the key value pairs in the query string.
-- For example,
-- > StaticRoute $ StaticR ["thumb001.jpg"] [("foo", "5"), ("bar", "choc")]
-- would generate a url such as 'http://site.com/static/thumb001.jpg?foo=5&bar=choc'
-- The StaticRoute constructor can be used when url's cannot be statically generated at compile-time.
-- E.g. When generating image galleries.
data StaticRoute = StaticRoute [String] [(String, String)]
    deriving (Eq, Show, Read)

type instance Route Static = StaticRoute

instance YesodSubSite Static master where
    getSubSite = Site
        { handleSite = \_ (StaticRoute ps _) m ->
                            case m of
                                "GET" -> Just $ do
                                    Static prefix <- getYesodSub
                                    req <- waiRequest
                                    res <- Trans.lift $ staticApp StaticSettings
                                            { ssFolder = prefix
                                            , ssIndices = []
                                            , ssListing = Just defaultListing
                                            , ssGetMimeType = return . defaultMimeTypeByExt
                                            } req
                                    sendWaiResponse res
                                _ -> Nothing
        , formatPathSegments = \(StaticRoute x y) -> (x, y)
        , parsePathSegments = \x -> Right $ StaticRoute x []
        }

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

notHidden :: FilePath -> Bool
notHidden ('.':_) = False
notHidden "tmp" = False
notHidden _ = True

getFileList :: FilePath -> IO [[String]]
getFileList = flip go id
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
staticFiles fp = do
    fs <- qRunIO $ getFileList fp
    concat `fmap` mapM go fs
  where
    replace' c
        | 'A' <= c && c <= 'Z' = c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
        | otherwise = '_'
    go f = do
        let name = mkName $ intercalate "_" $ map (map replace') f
        f' <- lift f
        let sr = ConE $ mkName "StaticRoute"
        hash <- qRunIO $ fmap base64md5 $ L.readFile $ fp ++ '/' : intercalate "/" f
        let qs = ListE [TupE [LitE $ StringL hash, ListE []]]
        return
            [ SigD name $ ConT ''Route `AppT` ConT ''Static
            , FunD name
                [ Clause [] (NormalB $ sr `AppE` f' `AppE` qs) []
                ]
            ]

#if TEST

testSuite :: Test
testSuite = testGroup "Yesod.Helpers.Static"
    [ testCase "get file list" caseGetFileList
    ]

caseGetFileList :: Assertion
caseGetFileList = do
    x <- getFileList "test"
    x @?= [["foo"], ["bar", "baz"]]

#endif

-- | md5-hashes the given lazy bytestring and returns the hash as
-- base64url-encoded string.
--
-- This function returns the first 8 characters of the hash.
base64md5 :: L.ByteString -> String
base64md5 = map go
          . take 8
          . S8.unpack
          . Data.ByteString.Base64.encode
          . Data.Serialize.encode
          . md5
  where
    go '+' = '-'
    go '/' = '_'
    go c   = c
