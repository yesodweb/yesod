{-# LANGUAGE DeriveDataTypeable #-}
module Yesod.Default.Main
    ( defaultMain
    , defaultDevelApp
    , defaultDevelAppWith
    ) where

import Yesod.Default.Config
import Yesod.Logger (Logger, makeLogger, logString, logLazyText, flushLogger)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Debug (debugHandle)

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
defaultMain :: AppEnv e => IO (AppConfig e) -> (AppConfig e -> Logger -> (Application -> IO ()) -> IO ()) -> IO ()
defaultMain load withSite = do
    config <- load
    logger <- makeLogger
    withSite config logger $ run (appPort config)

-- | Run your development app using the provided @'DefaultEnv'@ type
--
--   > withDevelAppPort :: Dynamic
--   > withDevelAppPort = toDyn $ defaultDevelApp withMySite
--
defaultDevelApp :: (AppConfig DefaultEnv -> Logger -> (Application -> IO ()) -> IO ())
                -> ((Int, Application) -> IO ())
                -> IO ()
defaultDevelApp = defaultDevelAppWith loadDevelopmentConfig

-- | Run your development app using a custom environment type and loader
--   function
--
--   > withDevelAppPort :: Dynamic
--   > withDevelAppPort = toDyn $ (defaultDevelAppWith customLoadAppConfig) withMySite
--
defaultDevelAppWith :: AppEnv e
                    => IO (AppConfig e) -- ^ A means to load your development @'AppConfig'@
                    -> (AppConfig e -> Logger -> (Application -> IO ()) -> IO ()) -- ^ Your @withMySite@ function
                    -> ((Int, Application) -> IO ()) -> IO ()
defaultDevelAppWith load withSite f = do
        conf   <- load
        logger <- makeLogger
        let p = appPort conf
        logString logger $ "Devel application launched, listening on port " ++ show p
        withSite conf logger $ \app -> f (p, debugHandle (logHandle logger) app)
        flushLogger logger

        where
            logHandle logger msg = logLazyText logger msg >> flushLogger logger
