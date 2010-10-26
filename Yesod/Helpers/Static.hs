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
    , fileLookupDir
    , staticFiles
      -- * Hashing
    , base64md5
#if TEST
    , testSuite
#endif
    ) where

import System.Directory
import Control.Monad
import Data.Maybe (fromMaybe)

import Yesod hiding (lift)
import Data.List (intercalate)
import Language.Haskell.TH.Syntax
import Web.Routes

import qualified Data.ByteString.Lazy as L
import Data.Digest.Pure.MD5
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Char8 as S8
import qualified Data.Serialize

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
#endif

-- | A function for looking up file contents. For serving from the file system,
-- see 'fileLookupDir'.
data Static = Static
    { staticLookup :: FilePath -> IO (Maybe (Either FilePath Content))
    -- | Mapping from file extension to content type. See 'typeByExt'.
    , staticTypes :: [(String, ContentType)]
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
                                "GET" -> Just $ fmap chooseRep $ getStaticRoute ps
                                _ -> Nothing
        , formatPathSegments = \(StaticRoute x y) -> (x, y)
        , parsePathSegments = \x -> Right $ StaticRoute x []
        }

-- | Lookup files in a specific directory.
--
-- If you are just using this in combination with the static subsite (you
-- probably are), the handler itself checks that no unsafe paths are being
-- requested. In particular, no path segments may begin with a single period,
-- so hidden files and parent directories are safe.
--
-- For the second argument to this function, you can just use 'typeByExt'.
fileLookupDir :: FilePath -> [(String, ContentType)] -> Static
fileLookupDir dir = Static $ \fp -> do
    let fp' = dir ++ '/' : fp
    exists <- doesFileExist fp'
    if exists
        then return $ Just $ Left fp'
        else return Nothing

getStaticRoute :: [String]
               -> GHandler Static master (ContentType, Content)
getStaticRoute fp' = do
    Static fl ctypes <- getYesodSub
    when (any isUnsafe fp') notFound
    let fp = intercalate "/" fp'
    content <- liftIO $ fl fp
    case content of
        Nothing -> notFound
        Just (Left fp'') -> do
            let ctype = fromMaybe typeOctet $ lookup (ext fp'') ctypes
            sendFile ctype fp''
        Just (Right bs) -> do
            let ctype = fromMaybe typeOctet $ lookup (ext fp) ctypes
            return (ctype, bs)
  where
    isUnsafe [] = True
    isUnsafe ('.':_) = True
    isUnsafe _ = False

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
    replace' '.' = '_'
    replace' '-' = '_'
    replace' c = c
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
