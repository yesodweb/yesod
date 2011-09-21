module Yesod.Config
    ( AppConfig(..)
    , PostgresConf(..)
    , SqliteConf(..)
    , MongoConf(..)
    , loadConfig
    , loadPostgresql
    , loadSqlite
    , loadMongo
    ) where

import Control.Monad (join, forM)
import Data.Maybe (fromMaybe)
import Data.Object
import Data.Object.Yaml

import Data.Text (Text)
import qualified Data.Text as T

-- | Dynamic per-environment configuration which can be loaded at
--   run-time negating the need to recompile between environments.
data AppConfig e = AppConfig
    { appEnv  :: e
    , appPort :: Int
    , appRoot :: Text
    } deriving (Show)

-- separate types means more code here, but it's easier to use in the
-- scaffold

-- | Information required to connect to a postgres database
data PostgresConf = PostgresConf
    { pgConnStr  :: Text
    , pgPoolSize :: Int
    }

-- | Information required to connect to a sqlite database
data SqliteConf = SqliteConf
    { sqlDatabase :: String
    , sqlPoolSize :: Int
    }

-- | Information required to connect to a mongo database
data MongoConf = MongoConf
    { mgDatabase :: String
    , mgHost     :: String
    , mgPoolSize :: Int
    }

-- | Load an @'AppConfig'@ from @config\/settings.yml@.
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
loadConfig :: Show e => e -> IO (AppConfig e)
loadConfig env = withYamlEnvironment "config/settings.yml" env $ \e -> do
    let mssl     = lookupScalar "ssl"     e
    let mhost    = lookupScalar "host"    e
    let mport    = lookupScalar "port"    e
    let mapproot = lookupScalar "approot" e

    -- set some default arguments
    let ssl = maybe False toBool mssl
    port <- safeRead $ fromMaybe (if ssl then "443" else "80") mport

    approot <- case (mhost, mapproot) of
        (_        , Just ar) -> Just ar
        (Just host, _      ) -> Just $ (if ssl then "https://" else "http://") ++ host ++ (addPort ssl port)
        _                    -> Nothing

    return $ AppConfig
        { appEnv  = env
        , appPort = port
        , appRoot = T.pack approot
        }

    where
        toBool :: String -> Bool
        toBool = (`elem` ["true", "TRUE", "yes", "YES", "Y", "1"])

        addPort :: Bool -> Int -> String
        addPort True  443 = ""
        addPort False 80  = ""
        addPort _     p   = ":" ++ show p

-- | Load a @'PostgresConf'@ from @config\/postgresql.yml@.
-- 
--   > Production:
--   >   user: jsmith
--   >   password: secret
--   >   host: localhost
--   >   port: 5432
--   >   database: some_db
--   >   poolsize: 100
--
loadPostgresql :: Show e => e -> IO PostgresConf
loadPostgresql env = withYamlEnvironment "config/postgresql.yml" env $ \e -> do
    db   <- lookupScalar "database" e
    pool <- safeRead =<< lookupScalar "poolsize" e

    -- TODO: default host/port?
    connparts <- forM ["user", "password", "host", "port"] $ \k -> do
        v <- lookupScalar k e
        return $ k ++ "=" ++ v ++ " "

    conn <- return $ concat connparts

    return $ PostgresConf (T.pack $ conn ++ " dbname=" ++ db) pool

-- | Load a @'SqliteConf'@ from @config\/sqlite.yml@.
--
--   > Production:
--   >   database: foo.s3db
--   >   poolsize: 100
--
loadSqlite :: Show e => e -> IO SqliteConf
loadSqlite env = withYamlEnvironment "config/sqlite.yml" env $ \e -> do
    db   <- lookupScalar "database" e
    pool <- safeRead =<< lookupScalar "poolsize" e

    return $ SqliteConf db pool

-- | Load a @'MongoConf'@ from @config\/mongoDB.yml@.
--
--   > Production:
--   >   database: some_db
--   >   host:     localhost
--   >   poolsize: 100
--
loadMongo :: Show e => e -> IO MongoConf
loadMongo env = withYamlEnvironment "config/mongoDB.yml" env $ \e -> do
    db   <- lookupScalar "database" e
    host <- lookupScalar "host"     e
    pool <- safeRead =<< lookupScalar "poolsize" e

    return $ MongoConf db host pool

-- | Loads the configuration block in the passed file named by the
--   passed environment, yeilds to the passed function as a mapping.
--
--   Errors in the case of a bad load or if your function returns
--   @Nothing@.
withYamlEnvironment :: (IsYamlScalar v, Show e)
                    => FilePath -- ^ the yaml file
                    -> e        -- ^ the environment you want to load
                    -> ([(String, Object String v)] -> Maybe a) -- ^ what to do with the mapping
                    -> IO a
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

-- | Returns Nothing if read fails
safeRead :: String -> Maybe Int
safeRead s = case reads s of
    (i, _):_ -> Just i
    []       -> Nothing
