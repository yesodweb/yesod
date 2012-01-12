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

-- | A yesod-provided @'AppEnv'@, allows for Development, Testing, and
--   Production environments
data DefaultEnv = Development
                | Testing
                | Staging
                | Production deriving (Read, Show, Enum, Bounded)

-- | Setup commandline arguments for environment and port
data ArgConfig = ArgConfig
    { environment :: String
    , port        :: Int
    } deriving Show

parseArgConfig :: IO ArgConfig
parseArgConfig = do
    args <- getArgs
    (portS, args') <- getPort id args
    port <-
        case reads portS of
            (i, _):_ -> return i
            [] -> error $ "Invalid port value: " ++ show portS
    case args of
        [e] -> return $ ArgConfig e port
        _ -> do
            pn <- getProgName
            putStrLn $ "Usage: " ++ pn ++ " <environment> [--port <port>]"
            exitFailure
  where
    getPort front [] = do
        env <- getEnvironment
        return (fromMaybe "0" $ lookup "PORT" env, front [])
    getPort front ("--port":p:rest) = return (p, front rest)
    getPort front ("-p":p:rest) = return (p, front rest)
    getPort front (arg:rest) = getPort (front . (arg:)) rest

-- | Load the app config from command line parameters
fromArgs :: (Read env, Show env)
         => (env -> Object -> Parser extra)
         -> IO (AppConfig env extra)
fromArgs getExtra = do
    args <- parseArgConfig

    env <-
        case reads $ capitalize $ environment args of
            (e, _):_ -> return e
            [] -> error $ "Invalid environment: " ++ environment args

    let cs = (configSettings env)
                { csParseExtra = getExtra
                }
    config <- loadConfig cs

    return $ if port args /= 0
                then config { appPort = port args }
                else config

    where
        capitalize [] = []
        capitalize (x:xs) = toUpper x : map toLower xs

-- | Load your development config (when using @'DefaultEnv'@)
loadDevelopmentConfig :: IO (AppConfig DefaultEnv ())
loadDevelopmentConfig = loadConfig $ configSettings Development

-- | Dynamic per-environment configuration which can be loaded at
--   run-time negating the need to recompile between environments.
data AppConfig environment extra = AppConfig
    { appEnv   :: environment
    , appPort  :: Int
    , appRoot  :: Text
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
--   >   -- ssl: will default false
--   >   -- approot: will default to "http://localhost:3000"
--
--   > -- typical outward-facing production box
--   > Production:
--   >   host: www.example.com
--   >
--   >   -- ssl: will default false
--   >   -- port: will default 80
--   >   -- approot: will default "http://www.example.com"
--
--   > -- maybe you're reverse proxying connections to the running app
--   > -- on some other port
--   > Production:
--   >   port: 8080
--   >   approot: "http://example.com"
--   >
--   > -- approot is specified so that the non-80 port is not appended
--   > -- automatically.
--
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

    let mssl     = lookupScalar "ssl"     m
    let mhost    = lookupScalar "host"    m
    mport <- parseMonad (\x -> x .: "port") m
    let mapproot = lookupScalar "approot" m

    extra <- parseMonad (parseExtra env) m

    -- set some default arguments
    let ssl = maybe False toBool mssl
    let port' = fromMaybe (if ssl then 443 else 80) mport

    approot <- case (mhost, mapproot) of
        (_        , Just ar) -> return ar
        (Just host, _      ) -> return $ T.concat
            [ if ssl then "https://" else "http://"
            , host
            , addPort ssl port'
            ]
        _ -> fail "You must supply either a host or approot"

    return $ AppConfig
        { appEnv   = env
        , appPort  = port'
        , appRoot  = approot
        , appExtra = extra
        }

    where
        lookupScalar k m =
            case M.lookup k m of
                Just (String t) -> return t
                Just _ -> fail $ "Invalid value for: " ++ show k
                Nothing -> fail $ "Not found: " ++ show k
        toBool :: Text -> Bool
        toBool = (`elem` ["true", "TRUE", "yes", "YES", "Y", "1"])

        addPort :: Bool -> Int -> Text
        addPort True  443 = ""
        addPort False 80  = ""
        addPort _     p   = T.pack $ ':' : show p

-- | Loads the configuration block in the passed file named by the
--   passed environment, yeilds to the passed function as a mapping.
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
