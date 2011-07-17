{-# LANGUAGE OverloadedStrings #-}
module Scaffold.Devel
    ( devel
    ) where

import qualified Distribution.Simple.Build as B
import Distribution.Simple.Configure (configure)
import Distribution.Simple.Setup (defaultConfigFlags, configConfigurationsFlags, configUserInstall, Flag (..), defaultBuildFlags, defaultCopyFlags, defaultRegisterFlags)
import Distribution.Simple.Utils (defaultPackageDesc, defaultHookedPackageDesc)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Verbosity (normal)
import Distribution.PackageDescription.Parse (readPackageDescription, readHookedBuildInfo)
import Distribution.PackageDescription (FlagName (FlagName), package, emptyHookedBuildInfo)
import Distribution.Simple.LocalBuildInfo (localPkgDescr)
import Scaffold.Build (getDeps, touchDeps, findHaskellFiles)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Debug (debug)
import Distribution.Text (display)
import Distribution.Simple.Install (install)
import Distribution.Simple.Register (register)
import Control.Concurrent (forkIO, threadDelay, ThreadId, killThread)
import Control.Exception (try, SomeException, finally)
import System.PosixCompat.Files (modificationTime, getFileStatus)
import qualified Data.Map as Map
import System.Posix.Types (EpochTime)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Network.Wai (Application, Response (ResponseBuilder), responseLBS)
import Network.HTTP.Types (status500)
import Control.Monad (when, forever)
import System.Process (runCommand, terminateProcess, getProcessExitCode, waitForProcess)
import qualified Data.IORef as I
import qualified Data.ByteString.Lazy.Char8 as L
import System.Directory (doesFileExist, removeFile)
import Distribution.Package (PackageName (..), pkgName)

appMessage :: L.ByteString -> IO ()
appMessage l = forever $ do
    run 3000 . const . return $ responseLBS status500 [("Content-Type", "text/plain")] l
    threadDelay 10000

swapApp :: I.IORef ThreadId -> IO ThreadId -> IO ()
swapApp i f = do
    I.readIORef i >>= killThread
    f >>= I.writeIORef i

devel :: ([String] -> IO ()) -- ^ configure command
      -> ([String] -> IO ()) -- ^ build command
      -> IO ()
devel conf build = do
    e <- doesFileExist "dist/devel-flag"
    when e $ removeFile "dist/devel-flag"
    listenThread <- forkIO (appMessage "Initializing, please wait") >>= I.newIORef

    cabal <- defaultPackageDesc normal
    gpd <- readPackageDescription normal cabal

    mhpd <- defaultHookedPackageDesc
    hooked <-
        case mhpd of
            Nothing -> return emptyHookedBuildInfo
            Just fp -> readHookedBuildInfo normal fp

    lbi <- configure (gpd, hooked) (defaultConfigFlags defaultProgramConfiguration)
        { configConfigurationsFlags = [(FlagName "devel", True)]
        , configUserInstall = Flag True
        }

    let myTry :: IO () -> IO ()
        myTry f = try f >>= \x -> case x of
                                    Left e -> swapApp listenThread $ forkIO $ appMessage $ L.pack $ show (e :: SomeException)
                                    Right y -> return y
    let getNewApp :: IO ()
        getNewApp = myTry $ do
            putStrLn "Rebuilding app"
            swapApp listenThread $ forkIO $ appMessage "Rebuilding your app, please wait"

            deps <- getDeps
            touchDeps deps

            B.build
                (localPkgDescr lbi)
                lbi
                defaultBuildFlags
                []

            install (localPkgDescr lbi) lbi defaultCopyFlags
            register (localPkgDescr lbi) lbi defaultRegisterFlags

            let PackageName pi' = pkgName $ package $ localPkgDescr lbi
            writeFile "dist/devel.hs" $ unlines
                [ "{-# LANGUAGE PackageImports #-}"
                , concat
                    [ "import \""
                    , pi'
                    , "\" Controller (withDevelApp)"
                    ]
                , "import Data.Dynamic (fromDynamic)"
                , "import Network.Wai.Handler.Warp (run)"
                , "import Network.Wai.Middleware.Debug (debug)"
                , "import Data.Maybe (fromJust)"
                , "import Control.Concurrent (forkIO)"
                , "import System.Directory (doesFileExist, removeFile)"
                , "import Control.Concurrent (threadDelay)"
                , ""
                , "main :: IO ()"
                , "main = do"
                , "    putStrLn \"Starting app\""
                , "    forkIO $ (fromJust $ fromDynamic withDevelApp) $ run 3000"
                , "    loop"
                , ""
                , "loop :: IO ()"
                , "loop = do"
                , "    threadDelay 100000"
                , "    e <- doesFileExist \"dist/devel-flag\""
                , "    if e then removeFile \"dist/devel-flag\" else loop"
                ]
            swapApp listenThread $ forkIO $ do
                putStrLn "Calling runghc..."
                ph <- runCommand "runghc dist/devel.hs"
                let forceType :: Either SomeException () -> ()
                    forceType = const ()
                fmap forceType $ try sleepForever
                writeFile "dist/devel-flag" ""
                putStrLn "Terminating external process"
                terminateProcess ph
                putStrLn "Process terminated"
                ec <- waitForProcess ph
                putStrLn $ "Exit code: " ++ show ec

    loop Map.empty getNewApp

sleepForever :: IO ()
sleepForever = forever $ threadDelay 1000000

type FileList = Map.Map FilePath EpochTime

getFileList :: IO FileList
getFileList = do
    files <- findHaskellFiles "."
    deps <- getDeps
    let files' = files ++ map fst (Map.toList deps)
    fmap Map.fromList $ flip mapM files' $ \f -> do
        fs <- getFileStatus f
        return (f, modificationTime fs)

loop :: FileList -> IO () -> IO ()
loop oldList getNewApp = do
    newList <- getFileList
    when (newList /= oldList) getNewApp
    threadDelay 1000000
    loop newList getNewApp

errApp :: String -> Application
errApp s _ = return $ ResponseBuilder status500 [("Content-Type", "text/plain")] $ fromString s
