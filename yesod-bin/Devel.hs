{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell   #-}
module Devel
    ( devel
    , DevelOpts(..)
    , DevelTermOpt(..)
    , defaultDevelOpts
    ) where

import qualified Distribution.Compiler                 as D
import qualified Distribution.ModuleName               as D
import qualified Distribution.PackageDescription       as D
import qualified Distribution.PackageDescription.Parse as D
import qualified Distribution.Simple.Configure         as D
import qualified Distribution.Simple.Program           as D
import qualified Distribution.Simple.Utils             as D
import qualified Distribution.Verbosity                as D

import           Control.Applicative                   ((<$>), (<*>))
import           Control.Concurrent                    (forkIO, threadDelay)
import           Control.Concurrent.MVar               (MVar, newEmptyMVar,
                                                        takeMVar, tryPutMVar)
import qualified Control.Exception                     as Ex
import           Control.Monad                         (forever, unless, void,
                                                        when, forM)
import           Control.Monad.IO.Class                (liftIO)
import           Control.Monad.Trans.State             (evalStateT, get)
import qualified Data.IORef                            as I

import qualified Data.ByteString.Lazy                  as LB
import           Data.Char                             (isNumber, isUpper)
import qualified Data.List                             as L
import qualified Data.Map                              as Map
import           Data.Maybe                            (fromMaybe)
import qualified Data.Set                              as Set

import           System.Directory
import           System.Environment                    (getEnvironment)
import           System.Exit                           (ExitCode (..),
                                                        exitFailure,
                                                        exitSuccess)
import           System.FilePath                       (dropExtension,
                                                        splitDirectories,
                                                        takeExtension, (</>))
import           System.FSNotify
import           System.IO                             (Handle)
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
                                                        recompDeps)
import           GhcBuild                              (buildPackage,
                                                        getBuildFlags, getPackageArgs)

import qualified Config                                as GHC
import           Data.Streaming.Network                (bindPortTCP)
import           Network                               (withSocketsDo)
import           Network.HTTP.Conduit                  (conduitManagerSettings, newManager)
import           Data.Default.Class                    (def)
import           Network.HTTP.ReverseProxy             (ProxyDest (ProxyDest),
                                                        waiProxyToSettings, wpsTimeout, wpsOnExc)
import qualified Network.HTTP.ReverseProxy             as ReverseProxy
import           Network.HTTP.Types                    (status200, status503)
import           Network.Socket                        (sClose)
import           Network.Wai                           (responseLBS, requestHeaders)
import           Network.Wai.Parse                     (parseHttpAccept)
import           Network.Wai.Handler.Warp              (run)
import           SrcLoc                                (Located)
import           Data.FileEmbed        (embedFile)

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

data DevelTermOpt = TerminateOnEnter | TerminateOnlyInterrupt
     deriving (Show, Eq)
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
      , useReverseProxy :: Bool
      , terminateWith :: DevelTermOpt
      } deriving (Show, Eq)

getBuildDir :: DevelOpts -> String
getBuildDir opts = fromMaybe "dist" (buildDir opts)

defaultDevelOpts :: DevelOpts
defaultDevelOpts = DevelOpts False False False (-1) Nothing Nothing Nothing 3000 10 True TerminateOnEnter

cabalProgram :: DevelOpts -> FilePath
cabalProgram opts | isCabalDev opts = "cabal-dev"
                  | otherwise       = "cabal"

-- | Run a reverse proxy from port 3000 to 3001. If there is no response on
-- 3001, give an appropriate message to the user.
reverseProxy :: DevelOpts -> I.IORef Int -> IO ()
reverseProxy opts iappPort = do
    manager <- newManager conduitManagerSettings
    let refreshHtml = LB.fromChunks $ return $(embedFile "refreshing.html")
    let onExc _ req
            | maybe False (("application/json" `elem`) . parseHttpAccept)
                (lookup "accept" $ requestHeaders req) =
                    return $ responseLBS status503
                        [ ("Retry-After", "1")
                        ]
                        "{\"message\":\"Recompiling\"}"
            | otherwise = return $ responseLBS status200
                [ ("content-type", "text/html")
                , ("Refresh", "1")
                ]
                refreshHtml

    let runProxy =
            run (develPort opts) $ waiProxyToSettings
                (const $ do
                    appPort <- liftIO $ I.readIORef iappPort
                    return $
                        ReverseProxy.WPRProxyDest
                        $ ProxyDest "127.0.0.1" appPort)
                def
                    { wpsOnExc = \e req f -> onExc e req >>= f
                    , wpsTimeout =
                        if proxyTimeout opts == 0
                            then Nothing
                            else Just (1000000 * proxyTimeout opts)
                    }
                manager
    loop runProxy `Ex.onException` exitFailure
  where
    loop proxy = forever $ do
        void proxy
        putStrLn "Reverse proxy stopped, but it shouldn't"
        threadDelay 1000000
        putStrLn "Restarting reverse proxy"

checkPort :: Int -> IO Bool
checkPort p = do
    es <- Ex.try $ bindPortTCP p "*4"
    case es of
        Left (_ :: Ex.IOException) -> return False
        Right s -> do
            sClose s
            return True

getPort :: DevelOpts -> Int -> IO Int
getPort opts _ | not (useReverseProxy opts) = return $ develPort opts
getPort _ p0 =
    loop p0
  where
    loop p = do
        avail <- checkPort p
        if avail then return p else loop (succ p)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c a = c >>= \res -> unless res a

devel :: DevelOpts -> [String] -> IO ()
devel opts passThroughArgs = withSocketsDo $ withManager $ \manager -> do
    unlessM (checkPort $ develPort opts) $ error "devel port unavailable"
    iappPort <- getPort opts 17834 >>= I.newIORef
    when (useReverseProxy opts) $ void $ forkIO $ reverseProxy opts iappPort
    develHsPath <- checkDevelFile
    writeLock opts

    let (terminator, after) = case terminateWith opts of
          TerminateOnEnter ->
              ("Press ENTER", void getLine)
          TerminateOnlyInterrupt ->  -- run for one year
              ("Interrupt", threadDelay $ 1000 * 1000 * 60 * 60 * 24 * 365)


    putStrLn $ "Yesod devel server. "  ++ terminator ++ " to quit"
    void $ forkIO $ do
      filesModified <- newEmptyMVar
      void $ forkIO $
        void $ watchTree manager "." (const True) (\_ -> void (tryPutMVar filesModified ()))
      evalStateT (mainOuterLoop develHsPath iappPort filesModified) Map.empty
    after
    writeLock opts
    exitSuccess
  where
    bd = getBuildDir opts

    -- outer loop re-reads the cabal file
    mainOuterLoop develHsPath iappPort filesModified = do
      ghcVer <- liftIO ghcVersion
      cabal  <- liftIO $ D.findPackageDesc "."
      gpd    <- liftIO $ D.readPackageDescription D.normal cabal
      ldar   <- liftIO lookupLdAr
      (hsSourceDirs, _) <- liftIO $ checkCabalFile gpd
      liftIO $ removeFileIfExists (bd </> "setup-config")
      c <- liftIO $ configure opts passThroughArgs
      if c then do
             -- these files contain the wrong data after the configure step,
             -- remove them to force a cabal build first
             liftIO $ mapM_ removeFileIfExists [ "yesod-devel/ghcargs.txt"
                                               , "yesod-devel/arargs.txt"
                                               , "yesod-devel/ldargs.txt"
                                               ]
             rebuild <- liftIO $ mkRebuild ghcVer cabal opts ldar
             mainInnerLoop develHsPath iappPort hsSourceDirs filesModified cabal rebuild
           else do
             liftIO (threadDelay 5000000)
             mainOuterLoop develHsPath iappPort filesModified

    -- inner loop rebuilds after files change
    mainInnerLoop develHsPath iappPort hsSourceDirs filesModified cabal rebuild = go
       where
         go = do
           _ <- recompDeps hsSourceDirs
           list <- liftIO $ getFileList hsSourceDirs [cabal]
           success <- liftIO rebuild
           pkgArgs <- liftIO (ghcPackageArgs opts)
           let devArgs = pkgArgs ++ [develHsPath]
           let loop list0 = do
                   (haskellFileChanged, list1) <- liftIO $
                       watchForChanges filesModified hsSourceDirs [cabal] list0 (eventTimeout opts)
                   anyTouched <- recompDeps hsSourceDirs
                   unless (anyTouched || haskellFileChanged) $ loop list1
           if not success
             then liftIO $ do
                   putStrLn "\x1b[1;31mBuild failure, pausing...\x1b[0m"
                   runBuildHook $ failHook opts
             else do
                   liftIO $ runBuildHook $ successHook opts
                   liftIO $ removeLock opts
                   liftIO $ putStrLn
                            $ if verbose opts then "Starting development server: runghc " ++ L.unwords devArgs
                                              else "Starting development server..."
                   env0 <- liftIO getEnvironment

                   -- get a new port for the new process to listen on
                   appPort <- liftIO $ I.readIORef iappPort >>= getPort opts . (+ 1)
                   liftIO $ I.writeIORef iappPort appPort

                   (_,_,_,ph) <- liftIO $ createProcess (proc "runghc" devArgs)
                        { env = Just $ Map.toList
                                     $ Map.insert "PORT" (show appPort)
                                     $ Map.insert "DISPLAY_PORT" (show $ develPort opts)
                                     $ Map.fromList env0
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
           if n then mainOuterLoop develHsPath iappPort filesModified else go

runBuildHook :: Maybe String -> IO ()
runBuildHook (Just s) = do
             ret <- system s
             case ret of
                  ExitFailure _ -> putStrLn ("Error executing hook: " ++ s)
                  _             -> return ()
runBuildHook Nothing = return ()

{-
   run `cabal configure' with our wrappers
-}
configure :: DevelOpts -> [String] -> IO Bool
configure opts extraArgs =
  checkExit =<< createProcess (proc (cabalProgram opts) $
                                 [ "configure"
                                 , "-flibrary-only"
                                 , "--disable-tests"
                                 , "--disable-benchmarks"
                                 , "-fdevel"
                                 , "--disable-library-profiling"
                                 , "--with-ld=yesod-ld-wrapper"
                                 , "--with-ghc=yesod-ghc-wrapper"
                                 , "--with-ar=yesod-ar-wrapper"
                                 , "--with-hc-pkg=ghc-pkg"
                                 ] ++ extraArgs
               )

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file = removeFile file `Ex.catch` handler
    where
      handler :: IOError -> IO ()
      handler e | isDoesNotExistError e = return ()
                | otherwise             = Ex.throw e

mkRebuild :: String -> FilePath -> DevelOpts -> (FilePath, FilePath) -> IO (IO Bool)
mkRebuild ghcVer cabalFile opts (ldPath, arPath)
  | GHC.cProjectVersion /= ghcVer =
       failWith "Yesod has been compiled with a different GHC version, please reinstall"
  | forceCabal opts               = return (rebuildCabal opts)
  | otherwise                     =
      return $ do
        ns <- mapM (cabalFile `isNewerThan`)
           [ "yesod-devel/ghcargs.txt", "yesod-devel/arargs.txt", "yesod-devel/ldargs.txt" ]
        if or ns
          then rebuildCabal opts
          else do
            bf <- getBuildFlags
            rebuildGhc bf ldPath arPath


rebuildGhc :: [Located String] -> FilePath -> FilePath -> IO Bool
rebuildGhc bf ld ar = do
  putStrLn "Rebuilding application... (using GHC API)"
  buildPackage bf ld ar

rebuildCabal :: DevelOpts -> IO Bool
rebuildCabal opts = do
  putStrLn $ "Rebuilding application... (using " ++ cabalProgram opts ++ ")"
  checkExit =<< createProcess (proc (cabalProgram opts) args)
    where
      args | verbose opts = [ "build" ]
           | otherwise    = [ "build", "-v0" ]

try_ :: forall a. IO a -> IO ()
try_ x = void (Ex.try x :: IO (Either Ex.SomeException a))

type FileList = Map.Map FilePath EpochTime

getFileList :: [FilePath] -> [FilePath] -> IO FileList
getFileList hsSourceDirs extraFiles = do
    (files, deps) <- getDeps hsSourceDirs
    let files' = extraFiles ++ files ++ map fst (Map.toList deps)
    fmap Map.fromList $ forM files' $ \f -> do
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

checkDevelFile :: IO FilePath
checkDevelFile =
    loop paths
  where
    paths = ["app/devel.hs", "devel.hs", "src/devel.hs"]

    loop [] = failWith $ "file devel.hs not found, checked: " ++ show paths
    loop (x:xs) = do
        e <- doesFileExist x
        if e
            then return x
            else loop xs

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
           when ("Application" `notElem` (map (last . D.components) $ D.exposedModules dLib)) $
                putStrLn "WARNING: no exposed module Application"
           return (hsSourceDirs, dLib)

failWith :: String -> IO a
failWith msg = do
    putStrLn $ "ERROR: " ++ msg
    exitFailure

checkFileList :: FileList -> D.Library -> [FilePath]
checkFileList fl lib = filter (not . isSetup) . filter isUnlisted . filter isSrcFile $ sourceFiles
  where
    al = allModules lib
    -- a file is only a possible 'module file' if all path pieces start with a capital letter
    sourceFiles = filter isSrcFile . map fst . Map.toList $ fl
    isSrcFile file = let dirs = filter (/=".") $ splitDirectories file
                     in  all (isUpper . head) dirs && (takeExtension file `elem` [".hs", ".lhs"])
    isUnlisted file = not (toModuleName file `Set.member` al)
    toModuleName = L.intercalate "." . filter (/=".") . splitDirectories . dropExtension

    isSetup "Setup.hs" = True
    isSetup "./Setup.hs" = True
    isSetup "Setup.lhs" = True
    isSetup "./Setup.lhs" = True
    isSetup _ = False

allModules :: D.Library -> Set.Set String
allModules lib = Set.fromList $ map toString $ D.exposedModules lib ++ (D.otherModules . D.libBuildInfo) lib
    where
      toString = L.intercalate "." . D.components

ghcVersion :: IO String
ghcVersion = fmap getNumber $ readProcess "runghc" ["--numeric-version", "0"] []
    where
      getNumber = filter (\x -> isNumber x || x == '.')

ghcPackageArgs :: DevelOpts -> IO [String]
ghcPackageArgs opts = getBuildFlags >>= getPackageArgs (buildDir opts)

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

-- | wait for process started by @createProcess@, return True for ExitSuccess
checkExit :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO Bool
checkExit (_,_,_,h) = (==ExitSuccess) <$> waitForProcess' h
