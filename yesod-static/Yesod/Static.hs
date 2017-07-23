{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
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
-- cookies. In that case, you may wish to use 'urlParamRenderOverride'
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
      -- * Combining CSS/JS
      -- $combining
    , combineStylesheets'
    , combineScripts'
      -- ** Settings
    , CombineSettings
    , csStaticDir
    , csCssPostProcess
    , csJsPostProcess
    , csCssPreProcess
    , csJsPreProcess
    , csCombinedFolder
      -- * Template Haskell helpers
    , staticFiles
    , staticFilesList
    , staticFilesMap
    , staticFilesMergeMap
    , publicFiles
      -- * Hashing
    , base64md5
      -- * Embed
    , embed
#ifdef TEST_EXPORT
    , getFileListPieces
#endif
    ) where

import System.Directory
import qualified System.FilePath as FP
import Control.Monad
import Data.FileEmbed (embedDir)

import Control.Monad.Trans.Resource (runResourceT)
import Yesod.Core
import Yesod.Core.Types

import Data.List (intercalate)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as TH

import Crypto.Hash.Conduit (hashFile, sinkHash)
import Crypto.Hash (MD5, Digest)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Trans.State

import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.IORef (readIORef, newIORef, writeIORef)
import Data.Char (isLower, isDigit)
import Data.List (foldl')
import qualified Data.ByteString as S
import System.PosixCompat.Files (getFileStatus, modificationTime)
import System.Posix.Types (EpochTime)
import Data.Conduit
import Data.Conduit.List (sourceList, consume)
import Data.Conduit.Binary (sourceFile)
import qualified Data.Conduit.Text as CT
import Data.Functor.Identity (runIdentity)
import System.FilePath ((</>), (<.>), takeDirectory)
import qualified System.FilePath as F
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Default
--import Text.Lucius (luciusRTMinified)

import Network.Wai (pathInfo)
import Network.Wai.Application.Static
    ( StaticSettings (..)
    , staticApp
    , webAppSettingsWithLookup
    , embeddedSettings
    )
import WaiAppStatic.Storage.Filesystem (ETagLookup)

-- | Type used for the subsite with static contents.
newtype Static = Static StaticSettings

type StaticRoute = Route Static

-- | Produce a default value of 'Static' for a given file
-- folder.
--
-- Does not have index files or directory listings.  The static
-- files' contents /must not/ change, however new files can be
-- added.
static :: FilePath -> IO Static
static dir = do
    hashLookup <- cachedETagLookup dir
    return $ Static $ webAppSettingsWithLookup dir hashLookup

-- | Same as 'static', but does not assumes that the files do not
-- change and checks their modification time whenever a request
-- is made.
staticDevel :: FilePath -> IO Static
staticDevel dir = do
    hashLookup <- cachedETagLookupDevel dir
    return $ Static $ webAppSettingsWithLookup dir hashLookup

-- | Produce a 'Static' based on embedding all of the static files' contents in the
-- executable at compile time.
--
-- You should use "Yesod.EmbeddedStatic" instead, it is much more powerful.
--
-- Nota Bene: if you replace the scaffolded 'static' call in Settings/StaticFiles.hs
-- you will need to change the scaffolded addStaticContent.  Otherwise, some of your
-- assets will be 404'ed.  This is because by default yesod will generate compile those
-- assets to @static/tmp@ which for 'static' is fine since they are served out of the
-- directory itself.  With embedded static, that will not work.
-- You can easily change @addStaticContent@ to @\_ _ _ -> return Nothing@ as a workaround.
-- This will cause yesod to embed those assets into the generated HTML file itself.
embed :: FilePath -> Q Exp
embed fp = [|Static (embeddedSettings $(embedDir fp))|]

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
instance ParseRoute Static where
    parseRoute (x, y) = Just $ StaticRoute x y

instance (MonadThrow m, MonadIO m, MonadBaseControl IO m)
  => YesodSubDispatch Static (HandlerT master m) where
    yesodSubDispatch YesodSubRunnerEnv {..} req =
        ysreParentRunner base ysreParentEnv (fmap ysreToParentRoute route) req
      where
        base = stripHandlerT handlert ysreGetSub ysreToParentRoute route
        route = Just $ StaticRoute (pathInfo req) []

        Static set = ysreGetSub $ yreSite $ ysreParentEnv
        handlert = sendWaiApplication $ staticApp set

notHidden :: FilePath -> Bool
notHidden "tmp" = False
notHidden s =
    case s of
        '.':_ -> False
        _ -> True

getFileListPieces :: FilePath -> IO [[String]]
getFileListPieces = flip evalStateT M.empty . flip go id
  where
    go :: String
       -> ([String] -> [String])
       -> StateT (M.Map String String) IO [[String]]
    go fp front = do
        allContents <- liftIO $ filter notHidden `fmap` getDirectoryContents fp
        let fullPath :: String -> String
            fullPath f = fp ++ '/' : f
        files <- liftIO $ filterM (doesFileExist . fullPath) allContents
        let files' = map (front . return) files
        files'' <- mapM dedupe files'
        dirs <- liftIO $ filterM (doesDirectoryExist . fullPath) allContents
        dirs' <- mapM (\f -> go (fullPath f) (front . (:) f)) dirs
        return $ concat $ files'' : dirs'

    -- Reuse data buffers for identical strings
    dedupe :: [String] -> StateT (M.Map String String) IO [String]
    dedupe = mapM dedupe'

    dedupe' :: String -> StateT (M.Map String String) IO String
    dedupe' s = do
        m <- get
        case M.lookup s m of
            Just s' -> return s'
            Nothing -> do
                put $ M.insert s s m
                return s

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
-- > js_script_js = StaticRoute ["js", "script.js"] []
--
-- Note that dots (@.@), dashes (@-@) and slashes (@\/@) are
-- replaced by underscores (@\_@) to create valid Haskell
-- identifiers.
staticFiles :: FilePath -> Q [Dec]
staticFiles dir = mkStaticFiles dir

-- | Same as 'staticFiles', but takes an explicit list of files
-- to create identifiers for. The files path given are relative
-- to the static folder. For example, to create routes for the
-- files @\"static\/js\/jquery.js\"@ and
-- @\"static\/css\/normalize.css\"@, you would use:
--
-- > staticFilesList "static" ["js/jquery.js", "css/normalize.css"]
--
-- This can be useful when you have a very large number of static
-- files, but only need to refer to a few of them from Haskell.
staticFilesList :: FilePath -> [FilePath] -> Q [Dec]
staticFilesList dir fs =
    mkStaticFilesList dir (map split fs) True
  where
    split :: FilePath -> [String]
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
publicFiles :: FilePath -> Q [Dec]
publicFiles dir = mkStaticFiles' dir False

-- | Similar to 'staticFilesList', but takes a mapping of
-- unmunged names to fingerprinted file names.
--
-- @since 1.5.3
staticFilesMap :: FilePath -> M.Map FilePath FilePath -> Q [Dec]
staticFilesMap fp m = mkStaticFilesList' fp (map splitBoth mapList) True
  where
    splitBoth (k, v) = (split k, split v)
    mapList = M.toList m
    split :: FilePath -> [String]
    split [] = []
    split x =
        let (a, b) = break (== '/') x
         in a : split (drop 1 b)

-- | Similar to 'staticFilesMergeMap', but also generates identifiers
-- for all files in the specified directory that don't have a
-- fingerprinted version.
--
-- @since 1.5.3
staticFilesMergeMap :: FilePath -> M.Map FilePath FilePath -> Q [Dec]
staticFilesMergeMap fp m = do
  fs <- qRunIO $ getFileListPieces fp
  let filesList = map FP.joinPath fs
      mergedMapList = M.toList $ foldl' (checkedInsert invertedMap) m filesList
  mkStaticFilesList' fp (map splitBoth mergedMapList) True
  where
    splitBoth (k, v) = (split k, split v)
    swap (x, y) = (y, x)
    mapList = M.toList m
    invertedMap = M.fromList $ map swap mapList
    split :: FilePath -> [String]
    split [] = []
    split x =
        let (a, b) = break (== '/') x
         in a : split (drop 1 b)
    -- We want to keep mappings for all files that are pre-fingerprinted,
    -- so this function checks against all of the existing fingerprinted files and
    -- only inserts a new mapping if it's not a fingerprinted file.
    checkedInsert
      :: M.Map FilePath FilePath -- inverted dictionary
      -> M.Map FilePath FilePath -- accumulating state
      -> FilePath
      -> M.Map FilePath FilePath
    checkedInsert iDict st p = if M.member p iDict
      then st
      else M.insert p p st

mkHashMap :: FilePath -> IO (M.Map FilePath S8.ByteString)
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
                             return (file, S8.pack h)

pathFromRawPieces :: FilePath -> [String] -> FilePath
pathFromRawPieces =
    foldl' append
  where
    append a b = a ++ '/' : b

cachedETagLookupDevel :: FilePath -> IO ETagLookup
cachedETagLookupDevel dir = do
    etags <- mkHashMap dir
    mtimeVar <- newIORef (M.empty :: M.Map FilePath EpochTime)
    return $ \f ->
      case M.lookup f etags of
        Nothing -> return Nothing
        Just checksum -> do
          fs <- getFileStatus f
          let newt = modificationTime fs
          mtimes <- readIORef mtimeVar
          oldt <- case M.lookup f mtimes of
            Nothing -> writeIORef mtimeVar (M.insert f newt mtimes) >> return newt
            Just oldt -> return oldt
          return $ if newt /= oldt then Nothing else Just checksum


cachedETagLookup :: FilePath -> IO ETagLookup
cachedETagLookup dir = do
    etags <- mkHashMap dir
    return $ (\f -> return $ M.lookup f etags)

mkStaticFiles :: FilePath -> Q [Dec]
mkStaticFiles fp = mkStaticFiles' fp True

mkStaticFiles' :: FilePath -- ^ static directory
               -> Bool     -- ^ append checksum query parameter
               -> Q [Dec]
mkStaticFiles' fp makeHash = do
    fs <- qRunIO $ getFileListPieces fp
    mkStaticFilesList fp fs makeHash

mkStaticFilesList
    :: FilePath -- ^ static directory
    -> [[String]] -- ^ list of files to create identifiers for
    -> Bool     -- ^ append checksum query parameter
    -> Q [Dec]
mkStaticFilesList fp fs makeHash = mkStaticFilesList' fp (zip fs fs) makeHash

mkStaticFilesList'
    :: FilePath -- ^ static directory
    -> [([String], [String])] -- ^ list of files to create identifiers for, where
                              -- the first argument of the tuple is the identifier
                              -- alias and the second is the actual file name
    -> Bool     -- ^ append checksum query parameter
    -> Q [Dec]
mkStaticFilesList' fp fs makeHash = do
    concat `fmap` mapM mkRoute fs
  where
    replace' c
        | 'A' <= c && c <= 'Z' = c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
        | otherwise = '_'
    mkRoute (alias, f) = do
        let name' = intercalate "_" $ map (map replace') alias
            routeName = mkName $
                case () of
                    ()
                        | null name' -> error "null-named file"
                        | isDigit (head name') -> '_' : name'
                        | isLower (head name') -> name'
                        | otherwise -> '_' : name'
        f' <- [|map pack $(TH.lift f)|]
        qs <- if makeHash
                    then do hash <- qRunIO $ base64md5File $ pathFromRawPieces fp f
                            [|[(pack "etag", pack $(TH.lift hash))]|]
                    else return $ ListE []
        return
            [ SigD routeName $ ConT ''StaticRoute
            , FunD routeName
                [ Clause [] (NormalB $ (ConE 'StaticRoute) `AppE` f' `AppE` qs) []
                ]
            ]

base64md5File :: FilePath -> IO String
base64md5File = fmap (base64 . encode) . hashFile
    where encode d = ByteArray.convert (d :: Digest MD5)

base64md5 :: L.ByteString -> String
base64md5 lbs =
            base64 $ encode
          $ runIdentity
          $ sourceList (L.toChunks lbs) $$ sinkHash
  where
    encode d = ByteArray.convert (d :: Digest MD5)

base64 :: S.ByteString -> String
base64 = map tr
       . take 8
       . S8.unpack
       . Data.ByteString.Base64.encode
  where
    tr '+' = '-'
    tr '/' = '_'
    tr c   = c

-- $combining
--
-- A common scenario on a site is the desire to include many external CSS and
-- Javascript files on every page. Doing so via the Widget functionality in
-- Yesod will work, but would also mean that the same content will be
-- downloaded many times. A better approach would be to combine all of these
-- files together into a single static file and serve that as a static resource
-- for every page. That resource can be cached on the client, and bandwidth
-- usage reduced.
--
-- This could be done as a manual process, but that becomes tedious. Instead,
-- you can use some Template Haskell code which will combine these files into a
-- single static file at compile time.

data CombineType = JS | CSS

combineStatics' :: CombineType
                -> CombineSettings
                -> [Route Static] -- ^ files to combine
                -> Q Exp
combineStatics' combineType CombineSettings {..} routes = do
    texts <- qRunIO $ runResourceT $ mapM_ yield fps $$ awaitForever readUTFFile =$ consume
    ltext <- qRunIO $ preProcess $ TL.fromChunks texts
    bs    <- qRunIO $ postProcess fps $ TLE.encodeUtf8 ltext
    let hash' = base64md5 bs
        suffix = csCombinedFolder </> hash' <.> extension
        fp = csStaticDir </> suffix
    qRunIO $ do
        createDirectoryIfMissing True $ takeDirectory fp
        L.writeFile fp bs
    let pieces = map T.unpack $ T.splitOn "/" $ T.pack suffix
    [|StaticRoute (map pack pieces) []|]
  where
    fps :: [FilePath]
    fps = map toFP routes
    toFP (StaticRoute pieces _) = csStaticDir </> F.joinPath (map T.unpack pieces)
    readUTFFile fp = sourceFile fp =$= CT.decode CT.utf8
    postProcess =
        case combineType of
            JS -> csJsPostProcess
            CSS -> csCssPostProcess
    preProcess =
        case combineType of
            JS -> csJsPreProcess
            CSS -> csCssPreProcess
    extension =
        case combineType of
            JS -> "js"
            CSS -> "css"

-- | Data type for holding all settings for combining files.
--
-- This data type is a settings type. For more information, see:
--
-- <http://www.yesodweb.com/book/settings-types>
--
-- Since 1.2.0
data CombineSettings = CombineSettings
    { csStaticDir :: FilePath
    -- ^ File path containing static files.
    --
    -- Default: static
    --
    -- Since 1.2.0
    , csCssPostProcess :: [FilePath] -> L.ByteString -> IO L.ByteString
    -- ^ Post processing to be performed on CSS files.
    --
    -- Default: Pass-through.
    --
    -- Since 1.2.0
    , csJsPostProcess :: [FilePath] -> L.ByteString -> IO L.ByteString
    -- ^ Post processing to be performed on Javascript files.
    --
    -- Default: Pass-through.
    --
    -- Since 1.2.0
    , csCssPreProcess :: TL.Text -> IO TL.Text
    -- ^ Pre-processing to be performed on CSS files.
    --
    -- Default: convert all occurences of /static/ to ../
    --
    -- Since 1.2.0
    , csJsPreProcess :: TL.Text -> IO TL.Text
    -- ^ Pre-processing to be performed on Javascript files.
    --
    -- Default: Pass-through.
    --
    -- Since 1.2.0
    , csCombinedFolder :: FilePath
    -- ^ Subfolder to put combined files into.
    --
    -- Default: combined
    --
    -- Since 1.2.0
    }

instance Default CombineSettings where
    def = CombineSettings
        { csStaticDir = "static"
        {- Disabled due to: https://github.com/yesodweb/yesod/issues/623
        , csCssPostProcess = \fps ->
              either (error . (errorIntro fps)) (return . TLE.encodeUtf8)
            . flip luciusRTMinified []
            . TLE.decodeUtf8
        -}
        , csCssPostProcess = const return
        , csJsPostProcess = const return
           -- FIXME The following borders on a hack. With combining of files,
           -- the final location of the CSS is no longer fixed, so relative
           -- references will break. Instead, we switched to using /static/
           -- absolute references. However, when served from a separate domain
           -- name, this will break too. The solution is that, during
           -- development, we keep /static/, and in the combining phase, we
           -- replace /static with a relative reference to the parent folder.
        , csCssPreProcess =
              return
            . TL.replace "'/static/" "'../"
            . TL.replace "\"/static/" "\"../"
        , csJsPreProcess = return
        , csCombinedFolder = "combined"
        }

liftRoutes :: [Route Static] -> Q Exp
liftRoutes =
    fmap ListE . mapM go
  where
    go :: Route Static -> Q Exp
    go (StaticRoute x y) = [|StaticRoute $(liftTexts x) $(liftPairs y)|]

    liftTexts = fmap ListE . mapM liftT
    liftT t = [|pack $(TH.lift $ T.unpack t)|]

    liftPairs = fmap ListE . mapM liftPair
    liftPair (x, y) = [|($(liftT x), $(liftT y))|]

-- | Combine multiple CSS files together. Common usage would be:
--
-- >>> combineStylesheets' development def 'StaticR [style1_css, style2_css]
--
-- Where @development@ is a variable in your site indicated whether you are in
-- development or production mode.
--
-- Since 1.2.0
combineStylesheets' :: Bool -- ^ development? if so, perform no combining
                    -> CombineSettings
                    -> Name -- ^ Static route constructor name, e.g. \'StaticR
                    -> [Route Static] -- ^ files to combine
                    -> Q Exp
combineStylesheets' development cs con routes
    | development = [| mapM_ (addStylesheet . $(return $ ConE con)) $(liftRoutes routes) |]
    | otherwise = [| addStylesheet $ $(return $ ConE con) $(combineStatics' CSS cs routes) |]


-- | Combine multiple JS files together. Common usage would be:
--
-- >>> combineScripts' development def 'StaticR [script1_js, script2_js]
--
-- Where @development@ is a variable in your site indicated whether you are in
-- development or production mode.
--
-- Since 1.2.0
combineScripts' :: Bool -- ^ development? if so, perform no combining
                -> CombineSettings
                -> Name -- ^ Static route constructor name, e.g. \'StaticR
                -> [Route Static] -- ^ files to combine
                -> Q Exp
combineScripts' development cs con routes
    | development = [| mapM_ (addScript . $(return $ ConE con)) $(liftRoutes routes) |]
    | otherwise = [| addScript $ $(return $ ConE con) $(combineStatics' JS cs routes) |]
