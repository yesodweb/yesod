{-# LANGUAGE DeriveDataTypeable #-}
module Yesod.Main
    ( defaultMain
    , fromArgs
    , fromArgsWith
    , defaultDevelApp
    , defaultDevelAppWith
    ) where

import Yesod.Logger (Logger, makeLogger, logString, logLazyText, flushLogger)
import Yesod.Settings (AppEnvironment(..), AppConfig(..), loadConfig)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Debug (debugHandle)
import System.Console.CmdArgs hiding (args)
import Data.Char (toUpper, toLower)

data ArgConfig = ArgConfig
    { environment :: String
    , port        :: Int
    } deriving (Show, Data, Typeable)

-- | Load an @'AppConfig'@ using the provided function, then start your
--   app via Warp on the configured port.
--
--   > -- main.hs
--   > import Application (withMySite)
--   > import Yesod.Main  (defaultMain, fromArgs)
--   >
--   > main :: IO ()
--   > main = defaultMain fromArgs withMySite
--
defaultMain :: IO AppConfig -> (AppConfig -> Logger -> (Application -> IO ()) -> IO ()) -> IO ()
defaultMain load withSite = do
    config <- load
    logger <- makeLogger
    withSite config logger $ run (appPort config)

-- | Call the @'Yesod.Settings.loadConfig'@ function for the environment
--   passed on the commandline (or the default, \"development\") and
--   override the port if passed.
fromArgs :: IO AppConfig
fromArgs = fromArgsWith loadConfig

-- | Same, but allows one to provide their own custom @'loadConfig'@
fromArgsWith :: (AppEnvironment -> IO AppConfig) -> IO AppConfig
fromArgsWith load = do
    args <- cmdArgs argConfig

    let env = read
            $ capitalize
            $ if environment args /= ""
                  then environment args
                  else "development"

    config <- load env

    return $ if port args /= 0
                then config { appPort = port args }
                else config

    where
        argConfig :: ArgConfig
        argConfig = ArgConfig
            { environment = def 
                &= help ("application environment, one of: " ++ (foldl1 (\a b -> a ++ ", " ++ b) environments))
                &= typ   "ENVIRONMENT"
            , port = def
                &= help "the port to listen on"
                &= typ  "PORT"
            }

        environments :: [String]
        environments = map ((map toLower) . show) ([minBound..maxBound] :: [AppEnvironment])

        capitalize [] = []
        capitalize (x:xs) = toUpper x : map toLower xs

-- | A default argument for use with yesod devel with debug logging
--   enabled. Uses @'Yesod.Settings.loadConfig'@ for the @'Development'@
--   environment.
--
--   > -- Application.hs
--   > 
--   > withDevelAppPort :: Dynamic
--   > withDevelAppPort = toDyn $ defaultDevelApp withMySite
--
defaultDevelApp :: (AppConfig -> Logger -> (Application -> IO ()) -> IO ())
                -> ((Int, Application) -> IO ())
                -> IO ()
defaultDevelApp = defaultDevelAppWith loadConfig

-- | Same, but allows one to provide their own cust @'loadConfig'@
defaultDevelAppWith :: (AppEnvironment -> IO AppConfig)
                    -> (AppConfig -> Logger -> (Application -> IO ()) -> IO ())
                    -> ((Int, Application) -> IO ())
                    -> IO ()
defaultDevelAppWith load withSite f = do
        conf   <- load Development
        logger <- makeLogger
        let p = appPort conf
        logString logger $ "Devel application launched, listening on port " ++ show p
        withSite conf logger $ \app -> f (p, debugHandle (logHandle logger) app)
        flushLogger logger

        where
            logHandle logger msg = logLazyText logger msg >> flushLogger logger
