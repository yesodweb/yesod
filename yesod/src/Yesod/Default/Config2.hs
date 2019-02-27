{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Some next-gen helper functions for the scaffolding's configuration system.
module Yesod.Default.Config2
    ( -- * Locally defined
      configSettingsYml
    , getDevSettings
    , develMainHelper
      -- * Re-exports from Data.Yaml.Config
    , applyCurrentEnv
    , getCurrentEnv
    , applyEnvValue
    , loadYamlSettings
    , loadYamlSettingsArgs
    , EnvUsage
    , ignoreEnv
    , useEnv
    , requireEnv
    , useCustomEnv
    , requireCustomEnv
      -- * For backwards compatibility
    , MergedValue (..)
    , loadAppSettings
    , loadAppSettingsArgs
    ) where


import Data.Yaml.Config

import Data.Aeson
import qualified Data.HashMap.Strict as H
import System.Environment (getEnvironment)
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Concurrent (forkIO, threadDelay)
import System.Exit (exitSuccess)
import System.Directory (doesFileExist)

#ifndef mingw32_HOST_OS
import System.Posix.Signals (installHandler, sigINT, Handler(Catch))
#endif

newtype MergedValue = MergedValue { getMergedValue :: Value }

instance Semigroup MergedValue where
    MergedValue x <> MergedValue y = MergedValue $ mergeValues x y

-- | Left biased
mergeValues :: Value -> Value -> Value
mergeValues (Object x) (Object y) = Object $ H.unionWith mergeValues x y
mergeValues x _ = x

-- | Load the settings from the following three sources:
--
-- * Run time config files
--
-- * Run time environment variables
--
-- * The default compile time config file
loadAppSettings
    :: FromJSON settings
    => [FilePath] -- ^ run time config files to use, earlier files have precedence
    -> [Value] -- ^ any other values to use, usually from compile time config. overridden by files
    -> EnvUsage
    -> IO settings
loadAppSettings = loadYamlSettings
{-# DEPRECATED loadAppSettings "Use loadYamlSettings" #-}

-- | Same as @loadAppSettings@, but get the list of runtime config files from
-- the command line arguments.
loadAppSettingsArgs
    :: FromJSON settings
    => [Value] -- ^ any other values to use, usually from compile time config. overridden by files
    -> EnvUsage -- ^ use environment variables
    -> IO settings
loadAppSettingsArgs = loadYamlSettingsArgs
{-# DEPRECATED loadAppSettingsArgs "Use loadYamlSettingsArgs" #-}

-- | Location of the default config file.
configSettingsYml :: FilePath
configSettingsYml = "config/settings.yml"

-- | Helper for getApplicationDev in the scaffolding. Looks up PORT and
-- DISPLAY_PORT and prints appropriate messages.
getDevSettings :: Settings -> IO Settings
getDevSettings settings = do
    env <- getEnvironment
    let p = fromMaybe (getPort settings) $ lookup "PORT" env >>= readMaybe
        pdisplay = fromMaybe p $ lookup "DISPLAY_PORT" env >>= readMaybe
    putStrLn $ "Devel application launched: http://localhost:" ++ show pdisplay
    return $ setPort p settings

-- | Helper for develMain in the scaffolding.
develMainHelper :: IO (Settings, Application) -> IO ()
develMainHelper getSettingsApp = do
#ifndef mingw32_HOST_OS
    _ <- installHandler sigINT (Catch $ return ()) Nothing
#endif

    putStrLn "Starting devel application"
    (settings, app) <- getSettingsApp
    _ <- forkIO $ runSettings settings app
    loop
  where
    loop :: IO ()
    loop = do
        threadDelay 100000
        e <- doesFileExist "yesod-devel/devel-terminate"
        if e then terminateDevel else loop

    terminateDevel :: IO ()
    terminateDevel = exitSuccess
