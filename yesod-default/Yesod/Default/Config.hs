{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Default.Config
    ( DefaultEnv (..)
    , fromArgs
    , fromArgsExtra
    , loadDevelopmentConfig

    -- reexport
    , AppConfig (..)
    , ConfigSettings (..)
    , configSettings
    , loadConfig
    , withYamlEnvironment
    ) where

import Data.Char (toUpper, toLower)
import System.Console.CmdArgs hiding (args)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (join)
import Data.Object
import Data.Object.Yaml
import Data.Maybe (fromMaybe)

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
    } deriving (Show, Data, Typeable)

-- | A default @'ArgConfig'@ if using the provided @'DefaultEnv'@ type.
defaultArgConfig :: ArgConfig
defaultArgConfig =
    ArgConfig
        { environment = def
            &= argPos 0
            &= help ("application environment, one of: " ++ environments)
            &= typ   "ENVIRONMENT"
        , port = def
            &= help "the port to listen on"
            &= typ  "PORT"
        }

    where
        environments :: String
        environments = foldl1 (\a b -> a ++ ", " ++ b)
                     . map ((map toLower) . show)
                     $ ([minBound..maxBound] :: [DefaultEnv])

-- | Load an @'AppConfig'@ using the @'DefaultEnv'@ environments from
--   commandline arguments.
fromArgs :: IO (AppConfig DefaultEnv ())
fromArgs = fromArgsExtra (const $ const $ return ())

-- | Same as 'fromArgs', but allows you to specify how to parse the 'appExtra'
-- record.
fromArgsExtra :: (DefaultEnv -> TextObject -> IO extra)
              -> IO (AppConfig DefaultEnv extra)
fromArgsExtra = fromArgsWith defaultArgConfig

fromArgsWith :: (Read env, Show env)
             => ArgConfig
             -> (env -> TextObject -> IO extra)
             -> IO (AppConfig env extra)
fromArgsWith argConfig getExtra = do
    args   <- cmdArgs argConfig

    env <-
        case reads $ capitalize $ environment args of
            (e, _):_ -> return e
            [] -> error $ "Invalid environment: " ++ environment args

    let cs = (configSettings env)
                { csLoadExtra = getExtra
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
    , csLoadExtra :: environment -> TextObject -> IO extra
    -- | Return the path to the YAML config file.
    , csFile :: environment -> IO FilePath
    -- | Get the sub-object (if relevant) from the given YAML source which
    -- contains the specific settings for the current environment.
    , csGetObject :: environment -> TextObject -> IO TextObject
    }

-- | Default config settings.
configSettings :: Show env => env -> ConfigSettings env ()
configSettings env0 = ConfigSettings
    { csEnv = env0
    , csLoadExtra = \_ _ -> return ()
    , csFile = \_ -> return "config/settings.yml"
    , csGetObject = \env obj -> do
        envs <- fromMapping obj
        let senv = show env
            tenv = T.pack senv
        maybe
            (error $ "Could not find environment: " ++ senv)
            return
            (lookup tenv envs)
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
loadConfig (ConfigSettings env loadExtra getFile getObject) = do
    fp <- getFile env
    topObj <- join $ decodeFile fp
    obj <- getObject env topObj

    m <- maybe (fail "Expected map") return $ fromMapping obj
    let mssl     = lookupScalar "ssl"     m
    let mhost    = lookupScalar "host"    m
    let mport    = lookupScalar "port"    m
    let mapproot = lookupScalar "approot" m

    extra <- loadExtra env obj

    -- set some default arguments
    let ssl = maybe False toBool mssl
    port' <- safeRead "port" $ fromMaybe (if ssl then "443" else "80") mport

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
        toBool :: Text -> Bool
        toBool = (`elem` ["true", "TRUE", "yes", "YES", "Y", "1"])

        addPort :: Bool -> Int -> Text
        addPort True  443 = ""
        addPort False 80  = ""
        addPort _     p   = T.pack $ ':' : show p

-- | Returns 'fail' if read fails
safeRead :: Monad m => String -> Text -> m Int
safeRead name' t = case reads s of
    (i, _):_ -> return i
    []       -> fail $ concat ["Invalid value for ", name', ": ", s]
  where
    s = T.unpack t

-- | Loads the configuration block in the passed file named by the
--   passed environment, yeilds to the passed function as a mapping.
--
--   Errors in the case of a bad load or if your function returns
--   @Nothing@.
withYamlEnvironment :: Show e
                    => FilePath -- ^ the yaml file
                    -> e        -- ^ the environment you want to load
                    -> (TextObject -> IO a) -- ^ what to do with the mapping
                    -> IO a
withYamlEnvironment fp env f = do
    obj <- join $ decodeFile fp
    envs <- fromMapping obj
    conf <- maybe (fail $ "Could not find environment: " ++ show env) return
          $ lookup (T.pack $ show env) envs
    f conf
