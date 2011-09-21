{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE QuasiQuotes                 #-}
module Yesod.Config
    ( AppConfig(..)
    , loadConfig
    , loadPostgresqlConnStr
    , loadSqliteConnStr
    , loadMongoConnParams
    ) where

import Control.Monad (join)
import Data.Maybe (fromMaybe)
import Data.Object
import Data.Object.Yaml
import Data.Text (Text)
--import Text.Shakespeare.Text (st)

import qualified Data.Text        as T

-- | Dynamic per-environment configuration which can be loaded at
--   run-time negating the need to recompile between environments.
data AppConfig e = AppConfig
    { appEnv  :: e
    , appPort :: Int
    , appRoot :: Text
    } deriving (Show)

-- | Dynamic per-environment database configuration which can be loaded
--   at run-time
data DbConfig = PostgresConf String String Int -- ^ Connection string, Database, Pool size
              | SqliteConf String Int          -- ^ Database, Pool size
              | MongoConf (String,String) Int  -- ^ (Database,Host), Pool size

-- | Load an @'AppConfig'@ from a YAML-formatted file located at
--   @config\/settings.yml@. @'show'@ will be called on the first
--   parameter to determine which environment block to load
loadConfig :: Show e => e -> IO (AppConfig e)
loadConfig env = withYamlEnvironment "config/settings.yml" env $ \e -> do
    let mssl     = lookupScalar "ssl"     e
    let mhost    = lookupScalar "host"    e
    let mport    = lookupScalar "port"    e
    let mapproot = lookupScalar "approot" e

    -- set some default arguments
    let ssl = toBool $ fromMaybe "false" mssl
    port <- safeRead $ fromMaybe (if ssl then "443" else "80") mport

    approot <- case (mhost, mapproot) of
        (_        , Just ar) -> Just ar
        (Just host, _      ) -> Just $ (if ssl then "http://" else "https://") ++ host ++ (addPort ssl port)
        _                    -> Nothing

    return $ AppConfig
        { appEnv  = env
        , appPort = port
        , appRoot = T.pack approot
        }

    where
        toBool :: String -> Bool
        toBool = (`elem` ["true", "TRUE", "yes", "YES", "Y", "1"])

        safeRead :: String -> Maybe Int
        safeRead = undefined

        addPort :: Bool -> Int -> String
        addPort True  443 = ""
        addPort False 80  = ""
        addPort _     p   = ":" ++ show p

loadPostgresqlConnStr :: Show e => e -> IO DbConfig
loadPostgresqlConnStr env = withYamlEnvironment "config/postgresql.yml" env $ \e -> do
    db   <- lookupScalar "database" e
    pool <- lookupScalar "poolsize" e

    -- TODO: the rest of this
    return $ PostgresConf "todo" db (read pool)

    {-connPart <- fmap T.concat $ (flip mapM) ["user", "password", "host", "port"] $ \key -> do-}
      {-value <- lookupScalar key settings-}
      {-return $ [st| #{key}=#{value} |]-}
    {-return $ [st|#{connPart} dbname=#{database}|]-}

loadSqliteConnStr :: Show e => e -> IO DbConfig
loadSqliteConnStr env = withYamlEnvironment "config/sqlite.yml" env $ \e -> do
    db   <- lookupScalar "database" e
    pool <- lookupScalar "poolsize" e

    -- TODO: safer read
    return $ SqliteConf db (read pool)

loadMongoConnParams :: Show e => e -> IO DbConfig
loadMongoConnParams env = withYamlEnvironment "config/mongoDB.yml" env $ \e -> do
    db   <- lookupScalar "database" e
    host <- lookupScalar "host"     e
    pool <- lookupScalar "poolsize" e

    -- TODO: safer read
    return $ MongoConf (db, host) (read pool)

-- TODO: type sig -- ghci and I disagree here...
withYamlEnvironment fp env f = do
    obj <- join $ decodeFile fp
    case go obj env of
        Just v  -> return v
        Nothing -> error $ fp ++ ": invalid configuration file."

    where
        go o e = do
            envs <- fromMapping o
            conf <- lookupMapping (show e) envs
            f conf
