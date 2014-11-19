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
    ) where

import Data.Monoid
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Text (Text, pack)
import System.Environment (getEnvironment, getArgs)
import Control.Arrow ((***))
import Control.Applicative ((<$>))
import Control.Monad (guard, forM)
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

#ifndef mingw32_HOST_OS
import System.Posix.Signals (installHandler, sigINT, Handler(Catch))
#endif

newtype MergedValue = MergedValue { getMergedValue :: Value }

instance Monoid MergedValue where
    mempty = MergedValue $ Object H.empty
    MergedValue x `mappend` MergedValue y = MergedValue $ mergeValues x y

-- | Left biased
mergeValues :: Value -> Value -> Value
mergeValues (Object x) (Object y) = Object $ H.unionWith mergeValues x y
mergeValues (Object x) y | H.null x = y
mergeValues x _ = x

applyEnv :: H.HashMap Text Text -> Value -> Value
applyEnv env =
    goV
  where
    goV (Object o) =
        case checkEnv o of
            Just (name, value) ->
                case H.lookup name env of
                    Nothing -> value
                    Just t -> matchType value t
            Nothing -> Object $ goV <$> o
    goV (Array a) = Array (goV <$> a)
    goV v = v

    checkEnv o = do
        guard $ H.size o == 2
        String name <- H.lookup "env" o
        value <- H.lookup "value" o
        return (name, value)

matchType :: Value -> Text -> Value
matchType (Number _) t = tryWrap Number t
matchType (Bool _) t = tryWrap Bool t
matchType _ t = String t

tryWrap :: FromJSON a => (a -> Value) -> Text -> Value
tryWrap con t =
    case Y.decode $ encodeUtf8 t of
        Nothing -> String t
        Just x -> con x

getCurrentEnv :: IO (H.HashMap Text Text)
getCurrentEnv = fmap (H.fromList . map (pack *** pack)) getEnvironment

applyCurrentEnv :: Value -> IO Value
applyCurrentEnv orig = flip applyEnv orig <$> getCurrentEnv

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
    -> Bool -- ^ use environment variables
    -> IO settings
loadAppSettings runTimeFiles compileValues useEnv = do
    runValues <- forM runTimeFiles $ \fp -> do
        eres <- Y.decodeFileEither fp
        case eres of
            Left e -> do
                putStrLn $ "Could not parse file as YAML: " ++ fp
                throwIO e
            Right value -> return value
    let value' = getMergedValue
               $ mconcat
               $ map MergedValue
               $ runValues ++ compileValues
    value <-
        if useEnv
            then applyCurrentEnv value'
            else return $ applyEnv mempty value'

    case fromJSON value of
        Error s -> error $ "Could not convert to AppSettings: " ++ s
        Success settings -> return settings

-- | Same as @loadAppSettings@, but get the list of runtime config files from
-- the command line arguments.
loadAppSettingsArgs
    :: FromJSON settings
    => [Value] -- ^ any other values to use, usually from compile time config. overridden by files
    -> Bool -- ^ use environment variables
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
