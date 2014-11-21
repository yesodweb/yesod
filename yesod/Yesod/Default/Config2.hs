{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Some next-gen helper functions for the scaffolding's configuration system.
module Yesod.Default.Config2
    ( MergedValue (..)
    , applyCurrentEnv
    , getCurrentEnv
    , applyEnv
    , loadAppSettings
    , loadAppSettingsArgs
    , configSettingsYml
    , getDevSettings
    , develMainHelper
    , makeYesodLogger
    , EnvUsage
    , ignoreEnv
    , useEnv
    , requireEnv
    , useCustomEnv
    , requireCustomEnv
    ) where

import Data.Monoid
import Data.Semigroup
import Data.List.NonEmpty (nonEmpty)
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Text (Text, pack)
import System.Environment (getEnvironment, getArgs)
import Control.Arrow ((***))
import Control.Applicative ((<$>))
import Control.Monad (forM)
import Control.Exception (throwIO)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Yaml as Y
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Safe (readMay)
import Data.Maybe (fromMaybe)
import Control.Concurrent (forkIO, threadDelay)
import System.Exit (exitSuccess)
import System.Directory (doesFileExist)
import Network.Wai.Logger (clockDateCacher)
import Yesod.Core.Types (Logger (Logger))
import System.Log.FastLogger (LoggerSet)
import qualified Data.Text as T

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

applyEnv :: Bool -- ^ require an environment variable to be present?
         -> H.HashMap Text Text -> Value -> Value
applyEnv requireEnv' env =
    goV
  where
    goV (Object o) = Object $ goV <$> o
    goV (Array a) = Array (goV <$> a)
    goV (String t1) = fromMaybe (String t1) $ do
        t2 <- T.stripPrefix "_env:" t1
        let (name, t3) = T.break (== ':') t2
        Just $ case H.lookup name env of
            Just val -> parseValue val
            Nothing ->
                case T.stripPrefix ":" t3 of
                    Just val | not requireEnv' -> parseValue val
                    _ -> Null
    goV v = v

    parseValue val = fromMaybe (String val) $ Y.decode $ encodeUtf8 val

getCurrentEnv :: IO (H.HashMap Text Text)
getCurrentEnv = fmap (H.fromList . map (pack *** pack)) getEnvironment

applyCurrentEnv :: Bool -- ^ require an environment variable to be present?
                -> Value -> IO Value
applyCurrentEnv requireEnv' orig = flip (applyEnv requireEnv') orig <$> getCurrentEnv

data EnvUsage = IgnoreEnv
              | UseEnv
              | RequireEnv
              | UseCustomEnv (H.HashMap Text Text)
              | RequireCustomEnv (H.HashMap Text Text)

ignoreEnv, useEnv, requireEnv :: EnvUsage
ignoreEnv = IgnoreEnv
useEnv = UseEnv
requireEnv = RequireEnv

useCustomEnv, requireCustomEnv :: H.HashMap Text Text -> EnvUsage
useCustomEnv = UseCustomEnv
requireCustomEnv = RequireCustomEnv

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
loadAppSettings runTimeFiles compileValues envUsage = do
    runValues <- forM runTimeFiles $ \fp -> do
        eres <- Y.decodeFileEither fp
        case eres of
            Left e -> do
                putStrLn $ "loadAppSettings: Could not parse file as YAML: " ++ fp
                throwIO e
            Right value -> return value

    value' <-
        case nonEmpty $ map MergedValue $ runValues ++ compileValues of
            Nothing -> error "loadAppSettings: No configuration provided"
            Just ne -> return $ getMergedValue $ sconcat ne
    value <-
        case envUsage of
            IgnoreEnv            -> return $ applyEnv        False mempty value'
            UseEnv               ->          applyCurrentEnv False        value'
            RequireEnv           ->          applyCurrentEnv True         value'
            UseCustomEnv env     -> return $ applyEnv        False env    value'
            RequireCustomEnv env -> return $ applyEnv        True  env    value'

    case fromJSON value of
        Error s -> error $ "Could not convert to AppSettings: " ++ s
        Success settings -> return settings

-- | Same as @loadAppSettings@, but get the list of runtime config files from
-- the command line arguments.
loadAppSettingsArgs
    :: FromJSON settings
    => [Value] -- ^ any other values to use, usually from compile time config. overridden by files
    -> EnvUsage -- ^ use environment variables
    -> IO settings
loadAppSettingsArgs values env = do
    args <- getArgs
    loadAppSettings args values env

-- | Location of the default config file.
configSettingsYml :: FilePath
configSettingsYml = "config/settings.yml"

-- | Helper for getApplicationDev in the scaffolding. Looks up PORT and
-- DISPLAY_PORT and prints appropriate messages.
getDevSettings :: Settings -> IO Settings
getDevSettings settings = do
    env <- getEnvironment
    let p = fromMaybe (getPort settings) $ lookup "PORT" env >>= readMay
        pdisplay = fromMaybe p $ lookup "DISPLAY_PORT" env >>= readMay
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

makeYesodLogger :: LoggerSet -> IO Logger
makeYesodLogger loggerSet' = do
    (getter, _) <- clockDateCacher
    return $! Yesod.Core.Types.Logger loggerSet' getter
