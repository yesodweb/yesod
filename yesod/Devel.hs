{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Devel
    ( devel
    , DevelOpts(..)
    , defaultDevelOpts
    ) where


import qualified Distribution.Compiler                 as D
import qualified Distribution.InstalledPackageInfo     as IPI
import qualified Distribution.ModuleName               as D
import qualified Distribution.Package                  as D
import qualified Distribution.PackageDescription       as D
import qualified Distribution.PackageDescription.Parse as D
import qualified Distribution.Simple.Build             as D
import qualified Distribution.Simple.Compiler          as D
import qualified Distribution.Simple.Configure         as D
import qualified Distribution.Simple.LocalBuildInfo    as D
import qualified Distribution.Simple.Program           as D
import qualified Distribution.Simple.Register          as D
import qualified Distribution.Simple.Setup             as DSS
import qualified Distribution.Simple.Utils             as D
import qualified Distribution.Verbosity                as D

import           Control.Applicative                   ((<$>), (<*>))
import           Control.Concurrent                    (forkIO, threadDelay)
import           Control.Concurrent.MVar               (MVar, newEmptyMVar,
                                                        takeMVar, tryPutMVar)
import qualified Control.Exception                     as Ex
import           Control.Monad                         (forever, unless, void,
                                                        when)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans.State             (evalStateT, get)
import qualified Data.IORef                            as I

import           Data.Char                             (isNumber, isUpper)
import qualified Data.List                             as L
import qualified Data.Map                              as Map
import           Data.Maybe                            (fromMaybe)
import qualified Data.Set                              as Set
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)

import           System.Directory
import           System.Environment                    (getEnvironment)
import           System.Exit                           (ExitCode (..),
                                                        exitFailure,
                                                        exitSuccess)
import           System.FilePath                       (dropExtension,
                                                        splitDirectories,
                                                        takeExtension, (</>))
import           System.FSNotify
import           System.IO.Error                       (isDoesNotExistError)
import           System.Posix.Types                    (EpochTime)
import           System.PosixCompat.Files              (getFileStatus,
                                                        modificationTime)
import           System.Process                        (ProcessHandle,
                                                        createProcess, env,
                                                        getProcessExitCode,
                                                        proc, readProcess,
                                                        system,
                                                        terminateProcess)
import           System.Timeout                        (timeout)

import           Build                                 (getDeps, isNewerThan,
                                                        recompDeps, safeReadFile)
import           GhcBuild                              (buildPackage,
                                                        getBuildFlags)

import qualified Config                                as GHC
import           Data.Conduit.Network                  (HostPreference (HostIPv4),
                                                        bindPort)
import           Network                               (withSocketsDo)
import           Network.HTTP.Conduit                  (def, newManager)
import           Network.HTTP.ReverseProxy             (ProxyDest (ProxyDest),
                                                        waiProxyToSettings, wpsTimeout, wpsOnExc)
import           Network.HTTP.Types                    (status200)
import           Network.Socket                        (sClose)
import           Network.Wai                           (responseLBS)
import           Network.Wai.Handler.Warp              (run)
import           SrcLoc                                (Located)

lockFile :: DevelOpts -> FilePath
lockFile _opts =  "yesod-devel/devel-terminate"

writeLock :: DevelOpts -> IO ()
writeLock opts = do
    createDirectoryIfMissing True "yesod-devel"
    writeFile (lockFile opts) ""
    createDirectoryIfMissing True "dist" -- for compatibility with old devel.hs
    writeFile "dist/devel-terminate" ""

removeLock :: DevelOpts -> IO ()
removeLock opts = do
    removeFileIfExists (lockFile opts)
    removeFileIfExists "dist/devel-terminate"  -- for compatibility with old devel.hs

data DevelOpts = DevelOpts
      { isCabalDev   :: Bool
      , forceCabal   :: Bool
      , verbose      :: Bool
      , eventTimeout :: Int -- negative value for no timeout
      , successHook  :: Maybe String
      , failHook     :: Maybe String
      , buildDir     :: Maybe String
      , develPort    :: Int
      , proxyTimeout :: Int
      } deriving (Show, Eq)

getBuildDir :: DevelOpts -> String
getBuildDir opts = fromMaybe "dist" (buildDir opts)

defaultDevelOpts :: DevelOpts
defaultDevelOpts = DevelOpts False False False (-1) Nothing Nothing Nothing 3000 10

-- | Run a reverse proxy from port 3000 to 3001. If there is no response on
-- 3001, give an appropriate message to the user.
reverseProxy :: DevelOpts -> I.IORef Int -> IO ()
reverseProxy opts iappPort = do
    manager <- newManager def
    let loop = forever $ do
            run (develPort opts) $ waiProxyToSettings
                (const $ do
                    appPort <- liftIO $ I.readIORef iappPort
                    return $ Right $ ProxyDest "127.0.0.1" appPort)
                def
                    { wpsOnExc = onExc
                    , wpsTimeout = Just (1000000 * proxyTimeout opts)
                    }
                manager
            putStrLn "Reverse proxy stopped, but it shouldn't"
            threadDelay 1000000
            putStrLn "Restarting reverse proxy"
    loop `Ex.onException` exitFailure
  where
    onExc _ _ = return $ responseLBS
        status200
        [ ("content-type", "text/html")
        , ("Refresh", "1")
        ]
        "<h1>App not ready, please refresh</h1>"

checkPort :: Int -> IO Bool
checkPort p = do
    es <- Ex.try $ bindPort p HostIPv4
    case es of
        Left (_ :: Ex.IOException) -> return False
        Right s -> do
            sClose s
            return True

getPort :: Int -> IO Int
getPort p = do
    avail <- checkPort p
    if avail then return p else getPort (succ p)

devel :: DevelOpts -> [String] -> IO ()
devel opts passThroughArgs = withSocketsDo $ withManager $ \manager -> do
    avail <- checkPort $ develPort opts
    unless avail $ error "devel port unavailable"
    iappPort <- getPort 17834 >>= I.newIORef
    _ <- forkIO $ reverseProxy opts iappPort
    checkDevelFile
    writeLock opts

    putStrLn "Yesod devel server. Press ENTER to quit"
    _ <- forkIO $ do
      filesModified <- newEmptyMVar
      watchTree manager "." (const True) (\_ -> void (tryPutMVar filesModified ()))
      evalStateT (mainOuterLoop iappPort filesModified) Map.empty
    _ <- getLine
    writeLock opts
    exitSuccess
  where
    bd = getBuildDir opts

    -- outer loop re-reads the cabal file
    mainOuterLoop iappPort filesModified = do
      ghcVer <- liftIO ghcVersion
      cabal  <- liftIO $ D.findPackageDesc "."
      gpd    <- liftIO $ D.readPackageDescription D.normal cabal
      ldar   <- liftIO lookupLdAr
      (hsSourceDirs, lib) <- liftIO $ checkCabalFile gpd
      liftIO $ removeFileIfExists (bd </> "setup-config")
      liftIO $ configure cabal ghcVer gpd opts
      liftIO $ removeFileIfExists "yesod-devel/ghcargs.txt"  -- these files contain the wrong data after
      liftIO $ removeFileIfExists "yesod-devel/arargs.txt"   -- the configure step, remove them to force
      liftIO $ removeFileIfExists "yesod-devel/ldargs.txt"   -- a cabal build first
      rebuild <- liftIO $ mkRebuild gpd ghcVer cabal opts ldar
      mainInnerLoop iappPort hsSourceDirs filesModified cabal gpd lib ghcVer rebuild

    -- inner loop rebuilds after files change
    mainInnerLoop iappPort hsSourceDirs filesModified cabal gpd lib ghcVer rebuild = go
       where
         go = do
           _ <- recompDeps hsSourceDirs
           list <- liftIO $ getFileList hsSourceDirs [cabal]
           success <- liftIO rebuild
           pkgArgs <- liftIO $ ghcPackageArgs opts ghcVer (D.packageDescription gpd) lib
           let devArgs = pkgArgs ++ ["devel.hs"] ++ passThroughArgs
           let loop list0 = do
                   (haskellFileChanged, list1) <- liftIO $ watchForChanges filesModified hsSourceDirs [cabal] list0 (eventTimeout opts)
                   anyTouched <- recompDeps hsSourceDirs
                   unless (anyTouched || haskellFileChanged) $ loop list1
           if not success
             then liftIO $ do
                   putStrLn "Build failure, pausing..."
                   runBuildHook $ failHook opts
             else do
                   liftIO $ runBuildHook $ successHook opts
                   liftIO $ removeLock opts
                   liftIO $ putStrLn
                            $ if verbose opts then "Starting development server: runghc " ++ L.unwords devArgs
                                              else "Starting development server..."
                   env0 <- liftIO getEnvironment

                   -- get a new port for the new process to listen on
                   appPort <- liftIO $ I.readIORef iappPort >>= getPort . (+ 1)
                   liftIO $ I.writeIORef iappPort appPort

                   (_,_,_,ph) <- liftIO $ createProcess (proc "runghc" devArgs)
                        { env = Just $ ("PORT", show appPort) : ("DISPLAY_PORT", show $ develPort opts) : env0
                        }
                   derefMap <- get
                   watchTid <- liftIO . forkIO . try_ $ flip evalStateT derefMap $ do
                      loop list
                      liftIO $ do
                         putStrLn "Stopping development server..."
                         writeLock opts
                         threadDelay 1000000
                         putStrLn "Terminating development server..."
                         terminateProcess ph
                   ec <- liftIO $ waitForProcess' ph
                   liftIO $ putStrLn $ "Exit code: " ++ show ec
                   liftIO $ Ex.throwTo watchTid (userError "process finished")
           loop list
           n <- liftIO $ cabal `isNewerThan` (bd </> "setup-config")
           if n then mainOuterLoop iappPort filesModified else go

runBuildHook :: Maybe String -> IO ()
runBuildHook (Just s) = do
             ret <- system s
             case ret of
                  ExitFailure _ -> putStrLn ("Error executing hook: " ++ s)
                  _             -> return ()
runBuildHook Nothing = return ()

{-
  configure with the built-in Cabal lib for non-cabal-dev, since
  otherwise we cannot read the configuration later

  cabal-dev uses the command-line tool, we can fall back to
  cabal-dev buildopts if required
-}
configure :: FilePath -> String -> D.GenericPackageDescription -> DevelOpts -> IO ()
configure _cabalFile ghcVer gpd opts = do
  lbi <- D.configure (gpd, hookedBuildInfo) configFlags
  D.writePersistBuildConfig (getBuildDir opts) lbi -- fixme we could keep this in memory instead of file
  where
    hookedBuildInfo = (Nothing, [])
    configFlags0 | forceCabal opts = config
                 | otherwise       = config
                       { DSS.configProgramPaths =
                             [ ("ar",  "yesod-ar-wrapper")
                             , ("ld", "yesod-ld-wrapper")
                             , ("ghc", "yesod-ghc-wrapper")
                             ]
                       , DSS.configHcPkg = DSS.Flag "ghc-pkg"
                       }
#if MIN_VERSION_Cabal(1,16,0)
    configFlags | isCabalDev opts = configFlags0
                       { DSS.configPackageDBs =
                          [ Nothing
                          , Just D.GlobalPackageDB
                          , Just cabalDevPackageDb
                          ]
                       }
#else
    configFlags | isCabalDev opts = configFlags0
                       { DSS.configPackageDB = DSS.Flag cabalDevPackageDb
                       }
#endif
                | otherwise  = configFlags0

    cabalDevPackageDb = D.SpecificPackageDB ("cabal-dev/packages-" ++ ghcVer ++ ".conf")


    config = (DSS.defaultConfigFlags D.defaultProgramConfiguration)
               { DSS.configConfigurationsFlags =
                     [ (D.FlagName "devel", True)  -- legacy
                     , (D.FlagName "library-only", True)
                     ]
               , DSS.configProfLib     = DSS.Flag False
               , DSS.configUserInstall = DSS.Flag True
               }

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file = removeFile file `Ex.catch` handler
    where
      handler :: IOError -> IO ()
      handler e | isDoesNotExistError e = return ()
                | otherwise             = Ex.throw e

mkRebuild :: D.GenericPackageDescription -> String -> FilePath -> DevelOpts -> (FilePath, FilePath) -> IO (IO Bool)
mkRebuild gpd ghcVer cabalFile opts (ldPath, arPath)
  | GHC.cProjectVersion /= ghcVer = failWith "Yesod has been compiled with a different GHC version, please reinstall"
  | forceCabal opts               = return (rebuildCabal gpd opts)
  | otherwise                     = do
      return $ do
        n1 <- cabalFile `isNewerThan` "yesod-devel/ghcargs.txt"
        n2 <- cabalFile `isNewerThan` "yesod-devel/arargs.txt"
        n3 <- cabalFile `isNewerThan` "yesod-devel/ldargs.txt"
        if n1 || n2 || n3
          then rebuildCabal gpd opts
          else do
            bf <- getBuildFlags
            rebuildGhc bf ldPath arPath


rebuildGhc :: [Located String] -> FilePath -> FilePath -> IO Bool
rebuildGhc bf ld ar = do
  putStrLn "Rebuilding application... (using GHC API)"
  buildPackage bf ld ar

rebuildCabal :: D.GenericPackageDescription -> DevelOpts -> IO Bool
rebuildCabal _gpd opts = do
       putStrLn $ "Rebuilding application... (using Cabal library)"
       lbi <- getPersistBuildConfig opts -- fixme we could cache this from the configure step
       let buildFlags | verbose opts = DSS.defaultBuildFlags
                      | otherwise    = DSS.defaultBuildFlags { DSS.buildVerbosity = DSS.Flag D.silent }
       tryBool $ D.build (D.localPkgDescr lbi) lbi buildFlags []

tryBool :: IO a -> IO Bool
tryBool a = (a >> return True) `Ex.catch` \(e::Ex.SomeException) -> do
  putStrLn $ "Exception: " ++ show e
  return False

try_ :: forall a. IO a -> IO ()
try_ x = (Ex.try x :: IO (Either Ex.SomeException a)) >> return ()

type FileList = Map.Map FilePath EpochTime

getFileList :: [FilePath] -> [FilePath] -> IO FileList
getFileList hsSourceDirs extraFiles = do
    (files, deps) <- getDeps hsSourceDirs
    let files' = extraFiles ++ files ++ map fst (Map.toList deps)
    fmap Map.fromList $ flip mapM files' $ \f -> do
        efs <- Ex.try $ getFileStatus f
        return $ case efs of
            Left (_ :: Ex.SomeException) -> (f, 0)
            Right fs -> (f, modificationTime fs)

-- | Returns @True@ if a .hs file changed.
watchForChanges :: MVar () -> [FilePath] -> [FilePath] -> FileList -> Int -> IO (Bool, FileList)
watchForChanges filesModified hsSourceDirs extraFiles list t = do
    newList <- getFileList hsSourceDirs extraFiles
    if list /= newList
      then do
        let haskellFileChanged = not $ Map.null $ Map.filterWithKey isHaskell $
                Map.differenceWith compareTimes newList list `Map.union`
                Map.differenceWith compareTimes list newList
        return (haskellFileChanged, newList)
      else timeout (1000000*t) (takeMVar filesModified) >>
           watchForChanges filesModified hsSourceDirs extraFiles list t
  where
    compareTimes x y
        | x == y = Nothing
        | otherwise = Just x

    isHaskell filename _ = takeExtension filename `elem` [".hs", ".lhs", ".hsc", ".cabal"]

checkDevelFile :: IO ()
checkDevelFile = do
  e <- doesFileExist "devel.hs"
  unless e $ failWith "file devel.hs not found"

checkCabalFile :: D.GenericPackageDescription -> IO ([FilePath], D.Library)
checkCabalFile gpd = case D.condLibrary gpd of
    Nothing -> failWith "incorrect cabal file, no library"
    Just ct ->
      case lookupDevelLib gpd ct of
        Nothing   ->
          failWith "no development flag found in your configuration file. Expected a 'library-only' flag or the older 'devel' flag"
        Just dLib -> do
           let hsSourceDirs = D.hsSourceDirs . D.libBuildInfo $ dLib
           fl <- getFileList hsSourceDirs []
           let unlisted = checkFileList fl dLib
           unless (null unlisted) $ do
                putStrLn "WARNING: the following source files are not listed in exposed-modules or other-modules:"
                mapM_ putStrLn unlisted
           when (D.fromString "Application" `notElem` D.exposedModules dLib) $
                putStrLn "WARNING: no exposed module Application"
           return (hsSourceDirs, dLib)

failWith :: String -> IO a
failWith msg = do
    putStrLn $ "ERROR: " ++ msg
    exitFailure

checkFileList :: FileList -> D.Library -> [FilePath]
checkFileList fl lib = filter isUnlisted . filter isSrcFile $ sourceFiles
  where
    al = allModules lib
    -- a file is only a possible 'module file' if all path pieces start with a capital letter
    sourceFiles = filter isSrcFile . map fst . Map.toList $ fl
    isSrcFile file = let dirs = filter (/=".") $ splitDirectories file
                     in  all (isUpper . head) dirs && (takeExtension file `elem` [".hs", ".lhs"])
    isUnlisted file = not (toModuleName file `Set.member` al)
    toModuleName = L.intercalate "." . filter (/=".") . splitDirectories . dropExtension

allModules :: D.Library -> Set.Set String
allModules lib = Set.fromList $ map toString $ D.exposedModules lib ++ (D.otherModules . D.libBuildInfo) lib
    where
      toString = L.intercalate "." . D.components

ghcVersion :: IO String
ghcVersion = fmap getNumber $ readProcess "runghc" ["--numeric-version", "0"] []
    where
      getNumber = filter (\x -> isNumber x || x == '.')

ghcPackageArgs :: DevelOpts -> String -> D.PackageDescription -> D.Library -> IO [String]
ghcPackageArgs opts ghcVer cabal lib = do
     lbi <- getPersistBuildConfig opts
     cbi <- fromMaybeErr errCbi (D.libraryConfig lbi)
     if isCabalDev opts
       then return ("-hide-all-packages" : "-no-user-package-conf" : inplaceConf
                       : selfPkgArg lbi : cabalDevConf : depArgs lbi cbi)
       else return ("-hide-all-packages" : inplaceConf : selfPkgArg lbi : depArgs lbi cbi)
      where
        selfPkgArg lbi  = pkgArg . D.inplacePackageId . D.package . D.localPkgDescr $ lbi
        pkgArg (D.InstalledPackageId pkgId) = "-package-id" ++ pkgId
        depArgs lbi cbi = map pkgArg (deps lbi cbi)
        deps lbi cbi    = let pkgInfo = D.inplaceInstalledPackageInfo "." (getBuildDir opts) cabal lib lbi cbi
                          in  IPI.depends $ pkgInfo
        errCbi          = "No library ComponentBuildInfo"
        cabalDevConf    = "-package-confcabal-dev/packages-" ++ ghcVer ++ ".conf"
        inplaceConf     = "-package-conf" ++ (getBuildDir opts</>"package.conf.inplace")

getPersistBuildConfig :: DevelOpts -> IO D.LocalBuildInfo
getPersistBuildConfig opts = fromRightErr errLbi =<< getPersistConfigLenient opts -- D.maybeGetPersistBuildConfig path
    where
        errLbi          = "Could not read BuildInfo file: " ++ D.localBuildInfoFile (getBuildDir opts) ++
                          "\nMake sure that cabal-install has been compiled with the same GHC version as yesod." ++
                          "\nand that the Cabal library used by GHC is the same version"

-- there can be slight differences in the cabal version, ignore those when loading the file as long as we can parse it
getPersistConfigLenient :: DevelOpts -> IO (Either String D.LocalBuildInfo)
getPersistConfigLenient opts = do
  let file = D.localBuildInfoFile (getBuildDir opts)
  exists <- doesFileExist file
  if not exists
    then return (Left $ "file does not exist: " ++ file)
    else do
      xs <- safeReadFile file
      case xs of
        Left e -> return $ Left $ show e
        Right bs ->
          return $ case lines $ T.unpack $ decodeUtf8With lenientDecode bs of
                     [_,l2]  -> -- two lines, header and serialized rest
                       case reads l2 of
                         [(bi,_)] -> Right bi
                         _        -> (Left "cannot parse contents")
                     _       -> (Left "not a valid header/content file")

fromMaybeErr :: String -> Maybe b -> IO b
fromMaybeErr err Nothing = failWith err
fromMaybeErr _  (Just x) = return x

fromRightErr :: String -> Either String b -> IO b
fromRightErr str (Left err) = failWith (str ++ "\n" ++ err)
fromRightErr _   (Right b)  = return b

lookupDevelLib :: D.GenericPackageDescription -> D.CondTree D.ConfVar c a -> Maybe a
lookupDevelLib gpd ct | found     = Just (D.condTreeData ct)
                      | otherwise = Nothing
  where
    flags = map (unFlagName . D.flagName) $ D.genPackageFlags gpd
    unFlagName (D.FlagName x) = x
    found = any (`elem` ["library-only", "devel"]) flags

-- location of `ld' and `ar' programs
lookupLdAr :: IO (FilePath, FilePath)
lookupLdAr = do
  mla <- lookupLdAr'
  case mla of
    Nothing -> failWith "Cannot determine location of `ar' or `ld' program"
    Just la -> return la

lookupLdAr' :: IO (Maybe (FilePath, FilePath))
lookupLdAr' = do
  (_, pgmc) <- D.configCompiler (Just D.GHC) Nothing Nothing D.defaultProgramConfiguration D.silent
  pgmc' <- D.configureAllKnownPrograms D.silent pgmc
  return $ (,) <$> look D.ldProgram pgmc' <*> look D.arProgram pgmc'
     where
       look pgm pdb = fmap D.programPath (D.lookupProgram pgm pdb)

-- | nonblocking version of @waitForProcess@
waitForProcess' :: ProcessHandle -> IO ExitCode
waitForProcess' pid = go
  where
    go = do
      mec <- getProcessExitCode pid
      case mec of
        Just ec -> return ec
        Nothing -> threadDelay 100000 >> go

