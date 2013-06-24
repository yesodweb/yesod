{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Yesod.Default.Config
    ( DefaultEnv (..)
    , fromArgs
    , loadDevelopmentConfig

    -- reexport
    , AppConfig (..)
    , ConfigSettings (..)
    , configSettings
    , loadConfig
    , withYamlEnvironment
    ) where

import Data.Char (toUpper, toLower)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as M
import System.Environment (getArgs, getProgName, getEnvironment)
import System.Exit (exitFailure)
import Data.Conduit.Network (HostPreference)
import Data.String (fromString)

-- | A yesod-provided @'AppEnv'@, allows for Development, Testing, and
--   Production environments
data DefaultEnv = Development
                | Testing
                | Staging
                | Production deriving (Read, Show, Enum, Bounded)

-- | Setup commandline arguments for environment and port
data ArgConfig env = ArgConfig
    { environment :: env
    , port        :: Int
    } deriving Show

parseArgConfig :: (Show env, Read env, Enum env, Bounded env) => IO (ArgConfig env)
parseArgConfig = do
    let envs = [minBound..maxBound]
    args <- getArgs
    (portS, args') <- getPort id args
    portI <-
        case reads portS of
            (i, _):_ -> return i
            [] -> error $ "Invalid port value: " ++ show portS
    case args' of
        [e] -> do
            case reads $ capitalize e of
                (e', _):_ -> return $ ArgConfig e' portI
                [] -> do
                    () <- error $ "Invalid environment, valid entries are: " ++ show envs
                    -- next line just provided to force the type of envs
                    return $ ArgConfig (head envs) 0
        _ -> do
            pn <- getProgName
            putStrLn $ "Usage: " ++ pn ++ " <environment> [--port <port>]"
            putStrLn $ "Valid environments: " ++ show envs
            exitFailure
  where
    getPort front [] = do
        env <- getEnvironment
        return (fromMaybe "0" $ lookup "PORT" env, front [])
    getPort front ("--port":p:rest) = return (p, front rest)
    getPort front ("-p":p:rest) = return (p, front rest)
    getPort front (arg:rest) = getPort (front . (arg:)) rest

    capitalize [] = []
    capitalize (x:xs) = toUpper x : map toLower xs

-- | Load the app config from command line parameters
fromArgs :: (Read env, Show env, Enum env, Bounded env)
         => (env -> Object -> Parser extra)
         -> IO (AppConfig env extra)
fromArgs getExtra = do
    args <- parseArgConfig

    let env = environment args

    let cs = (configSettings env)
                { csParseExtra = getExtra
                }
    config <- loadConfig cs

    env' <- getEnvironment
    let config' =
            case lookup "APPROOT" env' of
                Nothing -> config
                Just ar -> config { appRoot = T.pack ar }

    return $ if port args /= 0
                then config' { appPort = port args }
                else config'

-- | Load your development config (when using @'DefaultEnv'@)
loadDevelopmentConfig :: IO (AppConfig DefaultEnv ())
loadDevelopmentConfig = loadConfig $ configSettings Development

-- | Dynamic per-environment configuration which can be loaded at
--   run-time negating the need to recompile between environments.
data AppConfig environment extra = AppConfig
    { appEnv   :: environment
    , appPort  :: Int
    , appRoot  :: Text
    , appHost  :: HostPreference
    , appExtra :: extra
    } deriving (Show)

data ConfigSettings environment extra = ConfigSettings
    {
    -- | An arbitrary value, used below, to indicate the current running
    -- environment. Usually, you will use 'DefaultEnv' for this type.
       csEnv :: environment
    -- | Load any extra data, to be used by the application.
    , csParseExtra :: environment -> Object -> Parser extra
    -- | Return the path to the YAML config file.
    , csFile :: environment -> IO FilePath
    -- | Get the sub-object (if relevant) from the given YAML source which
    -- contains the specific settings for the current environment.
    , csGetObject :: environment -> Value -> IO Value
    }

-- | Default config settings.
configSettings :: Show env => env -> ConfigSettings env ()
configSettings env0 = ConfigSettings
    { csEnv = env0
    , csParseExtra = \_ _ -> return ()
    , csFile = \_ -> return "config/settings.yml"
    , csGetObject = \env v -> do
        envs <-
            case v of
                Object obj -> return obj
                _ -> fail "Expected Object"
        let senv = show env
            tenv = T.pack senv
        maybe
            (error $ "Could not find environment: " ++ senv)
            return
            (M.lookup tenv envs)
    }

-- | Load an @'AppConfig'@.
--
--   Some examples:
--
--   > -- typical local development
--   > Development:
--   >   host: localhost
--   >   port: 3000
--   >
--   >   -- approot: will default to ""
--
--   > -- typical outward-facing production box
--   > Production:
--   >   host: www.example.com
--   >
--   >   -- port: will default 80
--   >   -- host: will default to "*"
--   >   -- approot: will default "http://www.example.com"
--
--   > -- maybe you're reverse proxying connections to the running app
--   > -- on some other port
--   > Production:
--   >   port: 8080
--   >   approot: "http://example.com"
--   >   host: "localhost"
loadConfig :: ConfigSettings environment extra
           -> IO (AppConfig environment extra)
loadConfig (ConfigSettings env parseExtra getFile getObject) = do
    fp <- getFile env
    mtopObj <- decodeFile fp
    topObj <- maybe (fail "Invalid YAML file") return mtopObj
    obj <- getObject env topObj
    m <-
        case obj of
            Object m -> return m
            _ -> fail "Expected map"

    let host    = fromString $ T.unpack $ fromMaybe "*" $ lookupScalar "host"    m
    mport <- parseMonad (\x -> x .: "port") m
    let approot' = fromMaybe "" $ lookupScalar "approot" m

    -- Handle the DISPLAY_PORT environment variable for yesod devel
    approot <-
        case T.stripSuffix ":3000" approot' of
            Nothing -> return approot'
            Just prefix -> do
                envVars <- getEnvironment
                case lookup "DISPLAY_PORT" envVars of
                    Nothing -> return approot'
                    Just p -> return $ prefix `T.append` T.pack (':' : p)

    extra <- parseMonad (parseExtra env) m

    -- set some default arguments
    let port' = fromMaybe 80 mport

    return $ AppConfig
        { appEnv   = env
        , appPort  = port'
        , appRoot  = approot
        , appHost  = host
        , appExtra = extra
        }

    where
        lookupScalar k m =
            case M.lookup k m of
                Just (String t) -> return t
                Just _ -> fail $ "Invalid value for: " ++ show k
                Nothing -> fail $ "Not found: " ++ show k

-- | Loads the configuration block in the passed file named by the
--   passed environment, yields to the passed function as a mapping.
--
--   Errors in the case of a bad load or if your function returns
--   @Nothing@.
withYamlEnvironment :: Show e
                    => FilePath -- ^ the yaml file
                    -> e        -- ^ the environment you want to load
                    -> (Value -> Parser a) -- ^ what to do with the mapping
                    -> IO a
withYamlEnvironment fp env f = do
    mval <- decodeFile fp
    case mval of
        Nothing -> fail $ "Invalid YAML file: " ++ show fp
        Just (Object obj)
            | Just v <- M.lookup (T.pack $ show env) obj -> parseMonad f v
        _ -> fail $ "Could not find environment: " ++ show env
