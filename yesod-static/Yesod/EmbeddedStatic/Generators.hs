{-# LANGUAGE TemplateHaskell, QuasiQuotes, ScopedTypeVariables #-}
-- | A generator is executed at compile time to load a list of entries
-- to embed into the subsite.  This module contains several basic generators,
-- but the design of generators and entries is such that it is straightforward
-- to make custom generators for your own specific purposes, see <#g:4 this section>.
module Yesod.EmbeddedStatic.Generators (
  -- * Generators
    Location
  , embedFile
  , embedFileAt
  , embedDir
  , embedDirAt
  , concatFiles
  , concatFilesWith

  -- * Compression options for 'concatFilesWith'
  , jasmine
  , uglifyJs
  , yuiJavascript
  , yuiCSS
  , closureJs
  , compressTool
  , tryCompressTools

  -- * Util
  , pathToName
  
  -- * Custom Generators
  
  -- $example
) where

import Control.Applicative as A ((<$>), (<*>))
import Control.Exception (try, SomeException)
import Control.Monad (forM, when)
import Data.Char (isDigit, isLower)
import Data.Conduit (($$))
import Data.Default (def)
import Data.Maybe (isNothing)
import Language.Haskell.TH
import Network.Mime (defaultMimeLookup)
import System.Directory (doesDirectoryExist, getDirectoryContents, findExecutable)
import System.FilePath ((</>))
import Text.Jasmine (minifym)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.List as C
import Data.Conduit.Binary (sourceHandle)
import qualified Data.Text as T
import qualified System.Process as Proc
import System.Exit (ExitCode (ExitSuccess))
import Control.Concurrent.Async (Concurrently (..))
import System.IO (hClose)

import Yesod.EmbeddedStatic.Types

-- | Embed a single file.  Equivalent to passing the same string twice to 'embedFileAt'.
embedFile :: FilePath -> Generator
embedFile f = embedFileAt f f

-- | Embed a single file at a given location within the static subsite and generate a
--   route variable based on the location via 'pathToName'.  The @FilePath@ must be a relative
--   path to the directory in which you run @cabal build@.  During development, the file located
--   at this filepath will be reloaded on every request.  When compiling for production, the contents
--   of the file will be embedded into the executable and so the file does not need to be
--   distributed along with the executable.
embedFileAt :: Location -> FilePath -> Generator
embedFileAt loc f = do
    let mime = defaultMimeLookup $ T.pack f
    let entry = def {
                    ebHaskellName = Just $ pathToName loc
                  , ebLocation = loc
                  , ebMimeType = mime
                  , ebProductionContent = fmap BL.fromStrict (BS.readFile f)
                  , ebDevelReload = [| fmap BL.fromStrict
                                       (BS.readFile $(litE $ stringL f)) |]
                  }
    return [entry]

-- | List all files recursively in a directory
getRecursiveContents :: Location -- ^ The directory to search
                     -> FilePath   -- ^ The prefix to add to the filenames
                     -> IO [(Location,FilePath)]
getRecursiveContents prefix topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    let loc = if null prefix then name else prefix ++ "/" ++ name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents loc path
      else return [(loc, path)]
  return (concat paths)

-- | Embed all files in a directory into the static subsite.
-- 
-- Equivalent to passing the empty string as the location to 'embedDirAt',
-- so the directory path itself is not part of the resource locations (and so
-- also not part of the generated route variable names).
embedDir :: FilePath -> Generator
embedDir = embedDirAt ""

-- | Embed all files in a directory to a given location within the static subsite.
--
-- The directory tree rooted at the 'FilePath' (which must be relative to the directory in
-- which you run @cabal build@) is embedded into the static subsite at the given
-- location.  Also, route variables will be created based on the final location
-- of each file.  For example, if a directory \"static\" contains the files
--
-- * css/bootstrap.css
--
-- * js/jquery.js
--
-- * js/bootstrap.js
-- 
-- then @embedDirAt \"somefolder\" \"static\"@ will
--
-- * Make the file @static\/css\/bootstrap.css@ available at the location
--   @somefolder\/css\/bootstrap.css@ within the static subsite and similarly
--   for the other two files.
--
-- * Create variables @somefolder_css_bootstrap_css@, @somefolder_js_jquery_js@,
--   @somefolder_js_bootstrap_js@ all of type @Route EmbeddedStatic@.
--
-- * During development, the files will be reloaded on every request.  During
--   production, the contents of all files will be embedded into the executable.
--
-- * During development, files that are added to the directory while the server
--   is running will not be detected.  You need to recompile the module which
--   contains the call to @mkEmbeddedStatic@.  This will also generate new route
--   variables for the new files.
embedDirAt :: Location -> FilePath -> Generator
embedDirAt loc dir = do
    files <- runIO $ getRecursiveContents loc dir
    concat <$> mapM (uncurry embedFileAt) files

-- | Concatinate a list of files and embed it at the location.  Equivalent to passing @return@ to
--   'concatFilesWith'.
concatFiles :: Location -> [FilePath] -> Generator
concatFiles loc files = concatFilesWith loc return files

-- | Concatinate a list of files into a single 'BL.ByteString', run the resulting content through the given
--   function, embed it at the given location, and create a haskell variable name for the route based on
--   the location.
--
--   The processing function is only run when compiling for production, and the processing function is
--   executed at compile time.  During development, on every request the files listed are reloaded,
--   concatenated, and served as a single resource at the given location without being processed.
concatFilesWith :: Location -> (BL.ByteString -> IO BL.ByteString) -> [FilePath] -> Generator
concatFilesWith loc process files = do
    let load = do putStrLn $ "Creating " ++ loc
                  BL.concat <$> mapM BL.readFile files >>= process
        expFiles = listE $ map (litE . stringL) files
        expCt = [| BL.concat <$> mapM BL.readFile $expFiles |]
        mime = defaultMimeLookup $ T.pack loc
    return [def { ebHaskellName = Just $ pathToName loc
                , ebLocation = loc
                , ebMimeType = mime
                , ebProductionContent = load
                , ebDevelReload = expCt
                }]

-- | Convienient rexport of 'minifym' with a type signature to work with 'concatFilesWith'.
jasmine :: BL.ByteString -> IO BL.ByteString
jasmine ct = return $ either (const ct) id $ minifym ct

-- | Use <https://github.com/mishoo/UglifyJS2 UglifyJS2> to compress javascript.
-- Assumes @uglifyjs@ is located in the path and uses options @[\"-m\", \"-c\"]@
-- to both mangle and compress and the option \"-\" to cause uglifyjs to read from
-- standard input.
uglifyJs :: BL.ByteString -> IO BL.ByteString
uglifyJs = compressTool "uglifyjs" ["-", "-m", "-c"]

-- | Use <http://yui.github.io/yuicompressor/ YUI Compressor> to compress javascript.
-- Assumes a script @yuicompressor@ is located in the path.  If not, you can still
-- use something like
--
-- > compressTool "java" ["-jar", "/path/to/yuicompressor.jar", "--type", "js"]
yuiJavascript :: BL.ByteString -> IO BL.ByteString
yuiJavascript = compressTool "yuicompressor" ["--type", "js"]

-- | Use <http://yui.github.io/yuicompressor/ YUI Compressor> to compress CSS.
-- Assumes a script @yuicompressor@ is located in the path.
yuiCSS :: BL.ByteString -> IO BL.ByteString
yuiCSS = compressTool "yuicompressor" ["--type", "css"]

-- | Use <https://developers.google.com/closure/compiler/ Closure> to compress
-- javascript using the default options.  Assumes a script @closure@ is located in
-- the path. If not, you can still run using
--
-- > compressTool "java" ["-jar", "/path/to/compiler.jar"]
closureJs :: BL.ByteString -> IO BL.ByteString
closureJs = compressTool "closure" []

-- | Helper to convert a process into a compression function.  The process
-- should be set up to take input from standard input and write to standard output.
compressTool :: FilePath -- ^ program
             -> [String] -- ^ options
             -> BL.ByteString -> IO BL.ByteString
compressTool f opts ct = do
    mpath <- findExecutable f
    when (isNothing mpath) $
        fail $ "Unable to find " ++ f
    let p = (Proc.proc f opts)
                { Proc.std_in = Proc.CreatePipe
                , Proc.std_out = Proc.CreatePipe
                }
    (Just hin, Just hout, _, ph) <- Proc.createProcess p
    (compressed, (), code) <- runConcurrently $ (,,)
        A.<$> Concurrently (sourceHandle hout $$ C.consume)
        A.<*> Concurrently (BL.hPut hin ct >> hClose hin)
        A.<*> Concurrently (Proc.waitForProcess ph)
    if code == ExitSuccess
        then do
            putStrLn $ "Compressed successfully with " ++ f
            return $ BL.fromChunks compressed
        else error $ "compressTool: compression failed with " ++ f


-- | Try a list of processing functions (like the compressions above) one by one until
-- one succeeds (does not raise an exception).  Once a processing function succeeds,
-- none of the remaining functions are used.  If none succeeds, the input is just
-- returned unprocessed.  This is helpful if you are distributing
-- code on hackage and do not know what compressors the user will have installed.  You
-- can list several and they will be tried in order until one succeeds.
tryCompressTools :: [BL.ByteString -> IO BL.ByteString] -> BL.ByteString -> IO BL.ByteString
tryCompressTools [] x = return x
tryCompressTools (p:ps) x = do
    mres <- try $ p x
    case mres of
        Left (err :: SomeException) -> do
            putStrLn $ show err
            tryCompressTools ps x
        Right res -> return res

-- | Clean up a path to make it a valid haskell name by replacing all non-letters
--   and non-numbers by underscores.  In addition, if the path starts with a capital
--   letter or number add an initial underscore.
pathToName :: FilePath -> Name
pathToName f = routeName
    where
      replace c
        | 'A' <= c && c <= 'Z' = c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
        | otherwise = '_'
      name = map replace f
      routeName = mkName $
            case () of
                ()
                    | null name -> error "null-named file"
                    | isDigit (head name) -> '_' : name
                    | isLower (head name) -> name
                    | otherwise -> '_' : name


-- $example
-- Here is an example of creating your own custom generator.
-- Because of template haskell stage restrictions, you must define generators in a
-- different module from where you use them.  The following generator will embed a
-- JSON document that contains the compile time.
--
-- >{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
-- >module CompileTime where
-- >
-- >import Data.Aeson
-- >import Data.Default
-- >import Data.Time
-- >import Yesod.EmbeddedStatic.Generators
-- >import Yesod.EmbeddedStatic.Types
-- >import qualified Data.ByteString.Lazy as BL
-- >
-- >getTime :: IO BL.ByteString
-- >getTime = do
-- >    t <- getCurrentTime
-- >    return $ encode $
-- >        object [ "compile_time" .= show t ]
-- >
-- >timeGenerator :: Location -> Generator
-- >timeGenerator loc =
-- >    return $ [def
-- >        { ebHaskellName = Just $ pathToName loc
-- >        , ebLocation    = loc
-- >        , ebMimeType    = "application/json"
-- >        , ebProductionContent = getTime
-- >        , ebDevelReload = [| getTime |]
-- >        }]
--
-- Notice how the @getTime@ action is given as both 'ebProductionContent' and
-- 'ebDevelReload'.  The result is that during development, the @getTime@ action
-- will be re-executed on every request so the time returned will be different
-- for each reload.  When compiling for production, the @getTime@ action will
-- be executed once at compile time to produce the content to embed and never
-- called at runtime.
--
-- Here is a small example yesod program using this generator.  Try toggling
-- the development argument to @mkEmbeddedStatic@.
-- 
-- >{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies #-}
-- >module Main where
-- >
-- >import Yesod
-- >import Yesod.EmbeddedStatic
-- >import CompileTime (timeGenerator)
-- >
-- >mkEmbeddedStatic True "eStatic" [timeGenerator "compile-time.json"]
-- >
-- >-- The above will generate variables
-- >-- eStatic :: EmbeddedStatic
-- >-- compile_time_json :: Route EmbeddedStatic
-- >
-- >data MyApp = MyApp { getStatic :: EmbeddedStatic }
-- >
-- >mkYesod "MyApp" [parseRoutes|
-- >/ HomeR GET
-- >/static StaticR EmbeddedStatic getStatic
-- >|]
-- >
-- >instance Yesod MyApp
-- >
-- >getHomeR :: Handler Html
-- >getHomeR = defaultLayout $ [whamlet|
-- ><h1>Hello
-- ><p>Check the 
-- >    <a href=@{StaticR compile_time_json}>compile time
-- >|]
-- >
-- >main :: IO ()
-- >main = warp 3000 $ MyApp eStatic
