{-# LANGUAGE OverloadedStrings #-}
module Devel
    ( devel
    ) where

-- import qualified Distribution.Simple.Build as B
-- import Distribution.Simple.Configure (configure)
import Distribution.Simple (defaultMainArgs)
-- import Distribution.Simple.Setup (defaultConfigFlags, configConfigurationsFlags, configUserInstall, Flag (..), defaultBuildFlags, defaultCopyFlags, defaultRegisterFlags)
import Distribution.Simple.Utils (defaultPackageDesc, defaultHookedPackageDesc)
-- import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Verbosity (normal)
import Distribution.PackageDescription.Parse (readPackageDescription, readHookedBuildInfo)
import Distribution.PackageDescription (emptyHookedBuildInfo)
-- import Distribution.Simple.LocalBuildInfo (localPkgDescr)
import Build (getDeps, touchDeps, findHaskellFiles)
-- import Network.Wai.Handler.Warp (run)
-- import Network.Wai.Middleware.Debug (debug)
-- import Distribution.Text (display)
-- import Distribution.Simple.Install (install)
-- import Distribution.Simple.Register (register)
import Control.Concurrent (forkIO, threadDelay, ThreadId, killThread)
import Control.Exception (try, SomeException)
import System.PosixCompat.Files (modificationTime, getFileStatus)
import qualified Data.Map as Map
import System.Posix.Types (EpochTime)
-- import Blaze.ByteString.Builder.Char.Utf8 (fromString)
-- import Network.Wai (Application, Response (ResponseBuilder), responseLBS)
-- import Network.HTTP.Types (status500)
import Control.Monad (when, forever)
import System.Process (runCommand, terminateProcess, waitForProcess)
import qualified Data.IORef as I
import qualified Data.ByteString.Lazy.Char8 as L
import System.Directory (doesFileExist, removeFile, getDirectoryContents)
-- import Distribution.Package (PackageName (..), pkgName)
import Data.Maybe (mapMaybe)

appMessage :: L.ByteString -> IO ()
appMessage _ = forever $ do
    -- run 3000 . const . return $ responseLBS status500 [("Content-Type", "text/plain")] l
    threadDelay 10000

swapApp :: I.IORef ThreadId -> IO ThreadId -> IO ()
swapApp i f = do
    I.readIORef i >>= killThread
    f >>= I.writeIORef i

devel :: ([String] -> IO ()) -- ^ cabal
      -> IO ()
devel cabalCmd = do
    e <- doesFileExist "dist/devel-flag"
    when e $ removeFile "dist/devel-flag"
    listenThread <- forkIO (appMessage "Initializing, please wait") >>= I.newIORef

    cabal <- defaultPackageDesc normal
    _ <- readPackageDescription normal cabal

    mhpd <- defaultHookedPackageDesc
    _ <- case mhpd of
            Nothing -> return emptyHookedBuildInfo
            Just fp -> readHookedBuildInfo normal fp

    cabalCmd ["configure", "-fdevel"]

    let myTry :: IO () -> IO ()
        myTry f = try f >>= \x -> case x of
                                    Left err -> swapApp listenThread $ forkIO $ appMessage $ L.pack $ show (err :: SomeException)
                                    Right y -> return y
    let getNewApp :: IO ()
        getNewApp = myTry $ do
            putStrLn "Rebuilding app"
            swapApp listenThread $ forkIO $ appMessage "Rebuilding your app, please wait"

            deps <- getDeps
            touchDeps deps

            cabalCmd ["build"]
            defaultMainArgs ["install"]

            pi' <- getPackageName
            writeFile "dist/devel.hs" $ unlines
                [ "{-# LANGUAGE PackageImports #-}"
                , concat
                    [ "import \""
                    , pi'
                    , "\" Application (withDevelAppPort)"
                    ]
                , "import Data.Dynamic (fromDynamic)"
                , "import Network.Wai.Handler.Warp (run)"
                , "import Data.Maybe (fromJust)"
                , "import Control.Concurrent (forkIO)"
                , "import System.Directory (doesFileExist, removeFile)"
                , "import Control.Concurrent (threadDelay)"
                , ""
                , "main :: IO ()"
                , "main = do"
                , "    putStrLn \"Starting app\""
                , "    wdap <- return $ fromJust $ fromDynamic withDevelAppPort"
                , "    forkIO $ wdap $ \\(port, app) -> run port app"
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

{-
errApp :: String -> Application
errApp s _ = return $ ResponseBuilder status500 [("Content-Type", "text/plain")] $ fromString s
-}

getPackageName :: IO String
getPackageName = do
    xs <- getDirectoryContents "."
    case mapMaybe (toCabal . reverse) xs of
        [x] -> return x
        [] -> error "No cabal files found"
        _ -> error "Too many cabal files found"
  where
    toCabal ('l':'a':'b':'a':'c':'.':x) = Just $ reverse x
    toCabal _ = Nothing
