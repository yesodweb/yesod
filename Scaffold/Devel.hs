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
import System.Plugins (loadDynamic)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Devel
import Network.Wai.Middleware.Debug (debug)
import Data.Dynamic (fromDynamic)
import Distribution.Text (display)
import Distribution.Simple.Install (install)
import Distribution.Simple.Register (register)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (try, SomeException)
import System.PosixCompat.Files (modificationTime, getFileStatus)
import qualified Data.Map as Map
import System.Posix.Types (EpochTime)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Network.Wai (Application, Response (ResponseBuilder))
import Network.HTTP.Types (status500)
import Control.Monad (when)

devel :: IO ()
devel = do
    appHolder <- initAppHolder
    _ <- forkIO $ run 3000 $ debug $ toApp appHolder

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

    let myTry :: IO (Either String x) -> IO (Either String x)
        myTry f = try f >>= \x -> return $ case x of
                                    Left e -> Left $ show (e :: SomeException)
                                    Right y -> y
    let getNewApp :: IO (Either String WithAppRunner)
        getNewApp = myTry $ do
            deps <- getDeps
            touchDeps deps

            B.build
                (localPkgDescr lbi)
                lbi
                defaultBuildFlags
                []

            install (localPkgDescr lbi) lbi defaultCopyFlags
            register (localPkgDescr lbi) lbi defaultRegisterFlags

            let pi' = display $ package $ localPkgDescr lbi
            dyn <- loadDynamic (pi', "Controller", "withDevelApp")
            return $ case fmap fromDynamic dyn of
                Nothing -> Left "withDevelApp not found"
                Just Nothing -> Left "Not a withApp"
                Just (Just withApp) -> Right withApp

    loop Map.empty appHolder getNewApp

type FileList = Map.Map FilePath EpochTime

getFileList :: IO FileList
getFileList = do
    files <- findHaskellFiles "."
    deps <- getDeps
    let files' = files ++ map fst (Map.toList deps)
    fmap Map.fromList $ flip mapM files' $ \f -> do
        fs <- getFileStatus f
        return (f, modificationTime fs)

loop :: FileList -> AppHolder -> IO (Either String WithAppRunner) -> IO ()
loop oldList appHolder getNewApp = do
    newList <- getFileList
    when (newList /= oldList) $ do
        res <- getNewApp
        case res of
            Left s -> swapAppSimple (errApp s) appHolder
            Right x -> swapApp x appHolder
    threadDelay 1000000
    loop newList appHolder getNewApp

errApp :: String -> Application
errApp s _ = return $ ResponseBuilder status500 [("Content-Type", "text/plain")] $ fromString s
