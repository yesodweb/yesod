{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module Yesod.Default.Main
    ( defaultMain
    , defaultMainLog
    , defaultRunner
    , defaultDevelApp
    , LogFunc
    ) where

import Yesod.Default.Config
import Network.Wai (Application)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, setPort, setHost, setOnException)
import qualified Network.Wai.Handler.Warp as Warp
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import Network.Wai.Middleware.Gzip (gzip, GzipFiles (GzipCacheFolder), gzipFiles, def)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Jsonp (jsonp)
import Control.Monad (when)
import System.Environment (getEnvironment)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad.Logger (Loc, LogSource, LogLevel (LevelError), liftLoc)
import System.Log.FastLogger (LogStr, toLogStr)
import Language.Haskell.TH.Syntax (qLocation)

#ifndef WINDOWS
import qualified System.Posix.Signals as Signal
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
#endif

-- | Run your app, taking environment and port settings from the
--   commandline.
--
--   @'fromArgs'@ helps parse a custom configuration
--
--   > main :: IO ()
--   > main = defaultMain (fromArgs parseExtra) makeApplication
--
defaultMain :: IO (AppConfig env extra)
            -> (AppConfig env extra -> IO Application)
            -> IO ()
defaultMain load getApp = do
    config <- load
    app <- getApp config
    runSettings 
        ( setPort (appPort config)
        $ setHost (appHost config)
        $ defaultSettings
        ) app

type LogFunc = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | Same as @defaultMain@, but gets a logging function back as well as an
-- @Application@ to install Warp exception handlers.
--
-- Since 1.2.5
defaultMainLog :: IO (AppConfig env extra)
               -> (AppConfig env extra -> IO (Application, LogFunc))
               -> IO ()
defaultMainLog load getApp = do
    config <- load
    (app, logFunc) <- getApp config
    runSettings 
        ( setPort (appPort config)
        $ setHost (appHost config)
        $ setOnException (const $ \e -> when (shouldLog' e) $ logFunc
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
        $ defaultSettings
        ) app
  where
    shouldLog' = Warp.defaultShouldDisplayException

-- | Run your application continously, listening for SIGINT and exiting
--   when received
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
defaultDevelApp
    :: IO (AppConfig env extra) -- ^ A means to load your development @'AppConfig'@
    -> (AppConfig env extra -> IO Application) -- ^ Get your @Application@
    -> IO (Int, Application)
defaultDevelApp load getApp = do
    conf   <- load
    env <- getEnvironment
    let p = fromMaybe (appPort conf) $ lookup "PORT" env >>= readMaybe
        pdisplay = fromMaybe p $ lookup "DISPLAY_PORT" env >>= readMaybe
    putStrLn $ "Devel application launched: http://localhost:" ++ show pdisplay
    app <- getApp conf
    return (p, app)
