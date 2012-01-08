{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
---------------------------------------------------------
--
-- | Serve static files from a Yesod app.
--
-- This is great for developing your application, but also for a
-- dead-simple deployment.  Caching headers are automatically
-- taken care of.
--
-- If you are running a proxy server (like Apache or Nginx),
-- you may want to have that server do the static serving instead.
--
-- In fact, in an ideal setup you'll serve your static files from
-- a separate domain name to save time on transmitting
-- cookies. In that case, you may wish to use 'urlRenderOverride'
-- to redirect requests to this subsite to a separate domain
-- name.
--
-- Note that this module's static subsite ignores all files and
-- directories that are hidden by Unix conventions (i.e. start
-- with a dot, such as @\".ssh\"@) and the directory "tmp" on the
-- root of the directory with static files.
module Yesod.Static
    ( -- * Subsite
      Static (..)
    , Route (..)
    , StaticRoute
      -- * Smart constructor
    , static
    , staticDevel
    , embed
      -- * Template Haskell helpers
    , staticFiles
    , staticFilesList
    , publicFiles
      -- * Hashing
    , base64md5
#ifdef TEST
    , getFileListPieces
#endif
    ) where

import Prelude hiding (FilePath)
import qualified Prelude
import System.Directory
import Control.Monad
import Data.FileEmbed (embedDir)

import Yesod.Core hiding (lift)

import Data.List (intercalate)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Crypto.Conduit (hashFile, sinkHash)
import Crypto.Hash.MD5 (MD5)

import qualified Data.ByteString.Base64
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Serialize
import Data.Text (Text, pack)
import Data.Monoid (mempty)
import qualified Data.Map as M
import Data.IORef (readIORef, newIORef, writeIORef)
import Network.Wai (pathInfo, rawPathInfo, responseLBS)
import Data.Char (isLower, isDigit)
import Data.List (foldl')
import qualified Data.ByteString as S
import Network.HTTP.Types (status301)
import System.PosixCompat.Files (getFileStatus, modificationTime)
import System.Posix.Types (EpochTime)
import Data.Conduit (($$), runResourceT)
import Data.Conduit.List (sourceList)
import Control.Monad.ST (runST)

import Network.Wai.Application.Static
    ( StaticSettings (..)
    , defaultWebAppSettings
    , staticApp
    , embeddedLookup
    , toEmbedded
    , toFilePath
    , fromFilePath
    , FilePath
    , ETagLookup
    , webAppSettingsWithLookup
    )

-- | Type used for the subsite with static contents.
newtype Static = Static StaticSettings

type StaticRoute = Route Static

-- | Produce a default value of 'Static' for a given file
-- folder.
--
-- Does not have index files or directory listings.  The static
-- files' contents /must not/ change, however new files can be
-- added.
static :: Prelude.FilePath -> IO Static
static dir = do
    hashLookup <- cachedETagLookup dir
    return $ Static $ webAppSettingsWithLookup (toFilePath dir) hashLookup

-- | Same as 'static', but does not assumes that the files do not
-- change and checks their modification time whenever a request
-- is made.
staticDevel :: Prelude.FilePath -> IO Static
staticDevel dir = do
    hashLookup <- cachedETagLookupDevel dir
    return $ Static $ webAppSettingsWithLookup (toFilePath dir) hashLookup

-- | Produce a 'Static' based on embedding all of the static
-- files' contents in the executable at compile time.
embed :: Prelude.FilePath -> Q Exp
embed fp =
    [|Static (defaultWebAppSettings
        { ssFolder = embeddedLookup (toEmbedded $(embedDir fp))
        })|]

instance RenderRoute Static where
    -- | A route on the static subsite (see also 'staticFiles').
    --
    -- You may use this constructor directly to manually link to a
    -- static file.  The first argument is the sub-path to the file
    -- being served whereas the second argument is the key-value
    -- pairs in the query string.  For example,
    --
    -- > StaticRoute $ StaticR [\"thumb001.jpg\"] [(\"foo\", \"5\"), (\"bar\", \"choc\")]
    --
    -- would generate a url such as
    -- @http://www.example.com/static/thumb001.jpg?foo=5&bar=choc@
    -- The StaticRoute constructor can be used when the URL cannot be
    -- statically generated at compile-time (e.g. when generating
    -- image galleries).
    data Route Static = StaticRoute [Text] [(Text, Text)]
        deriving (Eq, Show, Read)
    renderRoute (StaticRoute x y) = (x, y)

instance Yesod master => YesodDispatch Static master where
    -- Need to append trailing slash to make relative links work
    yesodDispatch _ _ _ _ _ _ [] _ req =
        return $ responseLBS status301 [("Location", rawPathInfo req `S.append` "/")] ""

    yesodDispatch _ (Static set) _ _ _ _ textPieces  _ req =
        staticApp set req { pathInfo = textPieces }

notHidden :: Prelude.FilePath -> Bool
notHidden "tmp" = False
notHidden s =
    case s of
        '.':_ -> False
        _ -> True

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

-- | Template Haskell function that automatically creates routes
-- for all of your static files.
--
-- For example, if you used
--
-- > staticFiles "static/"
--
-- and you had files @\"static\/style.css\"@ and
-- @\"static\/js\/script.js\"@, then the following top-level
-- definitions would be created:
--
-- > style_css    = StaticRoute ["style.css"]    []
-- > js_script_js = StaticRoute ["js/script.js"] []
--
-- Note that dots (@.@), dashes (@-@) and slashes (@\/@) are
-- replaced by underscores (@\_@) to create valid Haskell
-- identifiers.
staticFiles :: Prelude.FilePath -> Q [Dec]
staticFiles dir = mkStaticFiles dir

-- | Same as 'staticFiles', but takes an explicit list of files
-- to create identifiers for. The files path given are relative
-- to the static folder. For example, to create routes for the
-- files @\"static/js/jquery.js\"@ and
-- @\"static/css/normalize.css\"@, you would use:
--
-- > staticFilesList \"static\" [\"js\/jquery.js\", \"css\/normalize.css\"]
--
-- This can be useful when you have a very large number of static
-- files, but only need to refer to a few of them from Haskell.
staticFilesList :: Prelude.FilePath -> [Prelude.FilePath] -> Q [Dec]
staticFilesList dir fs =
    mkStaticFilesList dir (map split fs) "StaticRoute" True
  where
    split :: Prelude.FilePath -> [String]
    split [] = []
    split x =
        let (a, b) = break (== '/') x
         in a : split (drop 1 b)

-- | Same as 'staticFiles', but doesn't append an ETag to the
-- query string.
--
-- Using 'publicFiles' will speed up the compilation, since there
-- won't be any need for hashing files during compile-time.
-- However, since the ETag ceases to be part of the URL, the
-- 'Static' subsite won't be able to set the expire date too far
-- on the future.  Browsers still will be able to cache the
-- contents, however they'll need send a request to the server to
-- see if their copy is up-to-date.
publicFiles :: Prelude.FilePath -> Q [Dec]
publicFiles dir = mkStaticFiles' dir "StaticRoute" False


mkHashMap :: Prelude.FilePath -> IO (M.Map FilePath S8.ByteString)
mkHashMap dir = do
    fs <- getFileListPieces dir
    hashAlist fs >>= return . M.fromList
  where
    hashAlist :: [[String]] -> IO [(FilePath, S8.ByteString)]
    hashAlist fs = mapM hashPair fs
      where
        hashPair :: [String] -> IO (FilePath, S8.ByteString)
        hashPair pieces = do let file = pathFromRawPieces dir pieces
                             h <- base64md5File file
                             return (toFilePath file, S8.pack h)

pathFromRawPieces :: Prelude.FilePath -> [String] -> Prelude.FilePath
pathFromRawPieces =
    foldl' append
  where
    append a b = a ++ '/' : b

cachedETagLookupDevel :: Prelude.FilePath -> IO ETagLookup
cachedETagLookupDevel dir = do
    etags <- mkHashMap dir
    mtimeVar <- newIORef (M.empty :: M.Map FilePath EpochTime)
    return $ \f ->
      case M.lookup f etags of
        Nothing -> return Nothing
        Just checksum -> do
          fs <- getFileStatus $ fromFilePath f
          let newt = modificationTime fs
          mtimes <- readIORef mtimeVar
          oldt <- case M.lookup f mtimes of
            Nothing -> writeIORef mtimeVar (M.insert f newt mtimes) >> return newt
            Just oldt -> return oldt
          return $ if newt /= oldt then Nothing else Just checksum


cachedETagLookup :: Prelude.FilePath -> IO ETagLookup
cachedETagLookup dir = do
    etags <- mkHashMap dir
    return $ (\f -> return $ M.lookup f etags)

mkStaticFiles :: Prelude.FilePath -> Q [Dec]
mkStaticFiles fp = mkStaticFiles' fp "StaticRoute" True

mkStaticFiles' :: Prelude.FilePath -- ^ static directory
               -> String   -- ^ route constructor "StaticRoute"
               -> Bool     -- ^ append checksum query parameter
               -> Q [Dec]
mkStaticFiles' fp routeConName makeHash = do
    fs <- qRunIO $ getFileListPieces fp
    mkStaticFilesList fp fs routeConName makeHash

mkStaticFilesList
    :: Prelude.FilePath -- ^ static directory
    -> [[String]] -- ^ list of files to create identifiers for
    -> String   -- ^ route constructor "StaticRoute"
    -> Bool     -- ^ append checksum query parameter
    -> Q [Dec]
mkStaticFilesList fp fs routeConName makeHash = do
    concat `fmap` mapM mkRoute fs
  where
    replace' c
        | 'A' <= c && c <= 'Z' = c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
        | otherwise = '_'
    mkRoute f = do
        let name' = intercalate "_" $ map (map replace') f
            routeName = mkName $
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
                            [|[(pack $(lift hash), mempty)]|]
                    else return $ ListE []
        return
            [ SigD routeName $ ConT route
            , FunD routeName
                [ Clause [] (NormalB $ (ConE route) `AppE` f' `AppE` qs) []
                ]
            ]

base64md5File :: Prelude.FilePath -> IO String
base64md5File = fmap (base64 . encode) . hashFile
    where encode d = Data.Serialize.encode (d :: MD5)

base64md5 :: L.ByteString -> String
base64md5 lbs =
            base64 $ encode
          $ runST $ runResourceT
          $ sourceList (L.toChunks lbs) $$ sinkHash
  where
    encode d = Data.Serialize.encode (d :: MD5)

base64 :: S.ByteString -> String
base64 = map tr
       . take 8
       . S8.unpack
       . Data.ByteString.Base64.encode
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
