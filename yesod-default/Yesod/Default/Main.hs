{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Yesod.Default.Main
    ( defaultMain
    , defaultRunner
    , defaultDevelApp
    ) where

import Yesod.Default.Config
import Yesod.Logger (Logger, makeDefaultLogger, logString, flushLogger)
import Network.Wai (Application)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort, settingsHost)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import Network.Wai.Middleware.Gzip (gzip, GzipFiles (GzipCacheFolder), gzipFiles, def)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Jsonp (jsonp)
import Control.Monad (when)

#ifndef WINDOWS
import qualified System.Posix.Signals as Signal
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
#endif

-- | Run your app, taking environment and port settings from the
--   commandline.
--
--   Use @'fromArgs'@ when using the provided @'DefaultEnv'@ type, or
--   @'fromArgsWith'@ when using a custom type
--
--   > main :: IO ()
--   > main = defaultMain fromArgs withMySite
--
--   or
--
--   > main :: IO ()
--   > main = defaultMain (fromArgsWith customArgConfig) withMySite
--
defaultMain :: (Show env, Read env)
            => IO (AppConfig env extra)
            -> (AppConfig env extra -> Logger -> IO Application)
            -> IO ()
defaultMain load getApp = do
    config <- load
    logger <- makeDefaultLogger
    app <- getApp config logger
    runSettings defaultSettings
        { settingsHost = "0.0.0.0"
        , settingsPort = appPort config
        } app

-- | Run your application continously, listening for SIGINT and exiting
--   when recieved
--
--   > withYourSite :: AppConfig DefaultEnv -> Logger -> (Application -> IO a) -> IO ()
--   > withYourSite conf logger f = do
--   >     Settings.withConnectionPool conf $ \p -> do
--   >         runConnectionPool (runMigration yourMigration) p
--   >         defaultRunner f $ YourSite conf logger p
defaultRunner :: (Application -> IO ()) -> Application -> IO ()
defaultRunner f app = do
    -- clear the .static-cache so we don't have stale content
    exists <- doesDirectoryExist staticCache
    when exists $ removeDirectoryRecursive staticCache
#ifdef WINDOWS
    f (middlewares app)
#else
    tid <- forkIO $ f (middlewares app) >> return ()
    flag <- newEmptyMVar
    _ <- Signal.installHandler Signal.sigINT (Signal.CatchOnce $ do
        putStrLn "Caught an interrupt"
        killThread tid
        putMVar flag ()) Nothing
    takeMVar flag
#endif
  where
    middlewares = gzip gset . jsonp . autohead

    gset = def { gzipFiles = GzipCacheFolder staticCache }
    staticCache = ".static-cache"

-- | Run your development app using a custom environment type and loader
--   function
--
--   > withDevelAppPort :: Dynamic
--   > withDevelAppPort = toDyn $ defaultDevelApp customLoadAppConfig withMySite
--
defaultDevelApp
    :: (Show env, Read env)
    => IO (AppConfig env extra) -- ^ A means to load your development @'AppConfig'@
    -> (AppConfig env extra -> Logger -> IO Application) -- ^ Get your @Application@
    -> ((Int, Application) -> IO ()) -> IO ()
defaultDevelApp load getApp f = do
    conf   <- load
    logger <- makeDefaultLogger
    let p = appPort conf
    logString logger $ "Devel application launched, listening on port " ++ show p
    app <- getApp conf logger
    f (p, app)
    flushLogger logger
