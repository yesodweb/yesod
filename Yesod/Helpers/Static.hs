{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
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
    , staticFiles
#if TEST
    , testSuite
#endif
    ) where

import System.Directory
import Control.Monad

import Yesod
import Data.List (intercalate)
import Language.Haskell.TH.Syntax

#if TEST
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
#endif

-- | A function for looking up file contents. For serving from the file system,
-- see 'fileLookupDir'.
data Static = Static (FilePath -> IO (Maybe (Either FilePath Content)))

$(mkYesodSub "Static" [] [$parseRoutes|
* StaticRoute GET
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

notHidden :: FilePath -> Bool
notHidden ('.':_) = False
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

staticFiles :: FilePath -> Q [Dec]
staticFiles fp = do
    fs <- qRunIO $ getFileList fp
    concat `fmap` mapM go fs
  where
    replace '.' = '_'
    replace c = c
    go f = do
        let name = mkName $ intercalate "_" $ map (map replace) f
        f' <- lift f
        let sr = ConE $ mkName "StaticRoute"
        return
            [ SigD name $ ConT ''StaticRoutes
            , FunD name
                [ Clause [] (NormalB $ sr `AppE` f') []
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
