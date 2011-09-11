module Yesod.Settings
    ( AppEnvironment(..)
    , AppConfig(..)
    , loadConfig
    ) where

import Control.Monad (join)
import Data.Object
import Data.Text (Text)

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
