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

data AppConfig = AppConfig
    { appEnv             :: AppEnvironment
    , appPort            :: Int
    , connectionPoolSize :: Int
    , appRoot            :: Text
    } deriving (Show)

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

loadPostgresqlConnStr :: AppEnvironment -> IO Text
loadPostgresqlConnStr env = do
    allSettings <- (join $ YAML.decodeFile ("config/postgresql.yml" :: String)) >>= fromMapping
    settings    <- lookupMapping (show env) allSettings
    database    <- lookupScalar "database" settings :: IO Text

    connPart <- fmap T.concat $ (flip mapM) ["user", "password", "host", "port"] $ \key -> do
      value <- lookupScalar key settings
      return $ [st| #{key}=#{value} |]
    return $ [st|#{connPart} dbname=#{database}|]

loadSqliteConnStr :: AppEnvironment -> IO Text
loadSqliteConnStr env = do
    allSettings <- (join $ YAML.decodeFile ("config/sqlite.yml" :: String)) >>= fromMapping
    settings    <- lookupMapping (show env) allSettings
    lookupScalar "database" settings

-- note: no type signature to avoid Persistent.MongoDB dep
--loadMongoConnParams :: AppEnvironment -> IO (Database, HostName)
loadMongoConnParams env = do
    allSettings <- (join $ YAML.decodeFile ("config/mongoDB.yml" :: String)) >>= fromMapping
    settings    <- lookupMapping (show env) allSettings
    database    <- lookupScalar "database" settings
    host        <- lookupScalar "host" settings
    return (database, host)
