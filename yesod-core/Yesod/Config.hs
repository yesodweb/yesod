{-# LANGUAGE OverloadedStrings #-}
module Yesod.Config
    {-# DEPRECATED "This code has been moved to yesod-default. This module will be removed in the next major version bump." #-}
    ( AppConfig(..)
    , loadConfig
    , withYamlEnvironment
    ) where

import Control.Monad (join)
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
loadConfig env = withYamlEnvironment "config/settings.yml" env $ \e' -> do
    e <- maybe (fail "Expected map") return $ fromMapping e'
    let mssl     = lookupScalar "ssl"     e
    let mhost    = lookupScalar "host"    e
    let mport    = lookupScalar "port"    e
    let mapproot = lookupScalar "approot" e

    -- set some default arguments
    let ssl = maybe False toBool mssl
    port <- safeRead "port" $ fromMaybe (if ssl then "443" else "80") mport

    approot <- case (mhost, mapproot) of
        (_        , Just ar) -> return ar
        (Just host, _      ) -> return $ T.concat
            [ if ssl then "https://" else "http://"
            , host
            , addPort ssl port
            ]
        _                    -> fail "You must supply either a host or approot"

    return $ AppConfig
        { appEnv  = env
        , appPort = port
        , appRoot = approot
        }

    where
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
                    -> (TextObject -> IO a) -- ^ what to do with the mapping
                    -> IO a
withYamlEnvironment fp env f = do
    obj <- join $ decodeFile fp
    envs <- fromMapping obj
    conf <- maybe (fail $ "Could not find environment: " ++ show env) return
          $ lookup (T.pack $ show env) envs
    f conf

-- | Returns 'fail' if read fails
safeRead :: Monad m => String -> Text -> m Int
safeRead name t = case reads s of
    (i, _):_ -> return i
    []       -> fail $ concat ["Invalid value for ", name, ": ", s]
  where
    s = T.unpack t
