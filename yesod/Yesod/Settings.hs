{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes                 #-}
module Yesod.Settings
    ( AppEnvironment(..)
    , AppConfig(..)
    , loadConfig
    , loadPostgresqlConnStr
    , loadSqliteConnStr
    , loadMongoConnParams
    ) where

import Control.Monad (join)
import Data.Object
import Data.Text (Text)
import Text.Shakespeare.Text (st)

import qualified Data.Object.Yaml as YAML
import qualified Data.Text        as T

data AppEnvironment = Development
                    | Test
                    | Staging
                    | Production
                    deriving (Eq, Show, Read, Enum, Bounded)

-- | Dynamic per-environment configuration which can be loaded at
--   run-time negating the need to recompile between environments.
data AppConfig = AppConfig
    { appEnv  :: AppEnvironment
    , appPort :: Int

    -- | Your application will keep a connection pool and take
    --   connections from there as necessary instead of continually
    --   creating new connections. This value gives the maximum number
    --   of connections to be open at a given time. If your application
    --   requests a connection when all connections are in use, that
    --   request will fail. Try to choose a number that will work well
    --   with the system resources available to you while providing
    --   enough connections for your expected load.
    --
    --   Connections are returned to the pool as quickly as possible by
    --   Yesod to avoid resource exhaustion. A connection is only
    --   considered in use while within a call to runDB.
    , connectionPoolSize :: Int

    -- | The base URL for your application. This will usually be
    --   different for development and production. Yesod automatically
    --   constructs URLs for you, so this value must be accurate to
    --   create valid links.
    --
    --   If your domain name was "yesod.com", you would probably want it
    --   to be:
    --
    --   > "http://yesod.com"
    --
    , appRoot :: Text
    } deriving (Show)

-- | Load an @'AppConfig'@ from a YAML-formatted file located at
--   @config\/settings.yml@.
loadConfig :: AppEnvironment -> IO AppConfig
loadConfig env = do
    allSettings <- (join $ YAML.decodeFile ("config/settings.yml" :: String)) >>= fromMapping
    settings    <- lookupMapping (show env) allSettings
    hostS       <- lookupScalar "host" settings
    port        <- fmap read $ lookupScalar "port" settings
    connectionPoolSizeS <- lookupScalar "connectionPoolSize" settings

    return $ AppConfig
        { appEnv  = env
        , appPort = port
        , appRoot = T.pack $ hostS ++ addPort port
        , connectionPoolSize = read connectionPoolSizeS
        }

    where
        addPort :: Int -> String
        addPort p = case env of
            Production -> ""
            _          -> ":" ++ show p

-- | Load Postgresql settings from a YAML-formatted file located at
--   @config\/postgresql.yml@.
loadPostgresqlConnStr :: AppEnvironment -> IO Text
loadPostgresqlConnStr env = do
    allSettings <- (join $ YAML.decodeFile ("config/postgresql.yml" :: String)) >>= fromMapping
    settings    <- lookupMapping (show env) allSettings
    database    <- lookupScalar "database" settings :: IO Text

    connPart <- fmap T.concat $ (flip mapM) ["user", "password", "host", "port"] $ \key -> do
      value <- lookupScalar key settings
      return $ [st| #{key}=#{value} |]
    return $ [st|#{connPart} dbname=#{database}|]

-- | Load Sqlite settings from a YAML-formatted file located at
--   @config\/sqlite.yml@.
loadSqliteConnStr :: AppEnvironment -> IO Text
loadSqliteConnStr env = do
    allSettings <- (join $ YAML.decodeFile ("config/sqlite.yml" :: String)) >>= fromMapping
    settings    <- lookupMapping (show env) allSettings
    lookupScalar "database" settings

-- note: no type signature to avoid an extra Persistent.MongoDB dep for
-- those that don't need it
--loadMongoConnParams :: AppEnvironment -> IO (Database, HostName)
loadMongoConnParams env = do
    allSettings <- (join $ YAML.decodeFile ("config/mongoDB.yml" :: String)) >>= fromMapping
    settings    <- lookupMapping (show env) allSettings
    database    <- lookupScalar "database" settings
    host        <- lookupScalar "host" settings
    return (database, host)
