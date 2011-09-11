{-# LANGUAGE DeriveDataTypeable #-}
module Yesod.Main
    ( defaultMain
    , fromArgs
    , fromArgsWith
    ) where

import Yesod.Logger (Logger, makeLogger)
import Yesod.Settings (AppEnvironment(..), AppConfig(..), loadConfig)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
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
