{-# LANGUAGE DeriveDataTypeable #-}
module Yesod.Default.Config
    ( DefaultEnv(..)
    , ArgConfig(..)
    , defaultArgConfig
    , fromArgs
    , fromArgsWith
    , loadDevelopmentConfig

    -- reexport
    , module Yesod.Config
    ) where

import Yesod.Config
import Data.Char (toUpper, toLower)
import System.Console.CmdArgs hiding (args)

-- | A yesod-provided @'AppEnv'@, allows for Development, Testing, and
--   Production environments
data DefaultEnv = Development
                | Testing
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
            &= opt "development"
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
fromArgs :: IO (AppConfig DefaultEnv)
fromArgs = fromArgsWith defaultArgConfig

fromArgsWith :: (Read e, Show e) => ArgConfig -> IO (AppConfig e)
fromArgsWith argConfig = do
    args   <- cmdArgs argConfig

    let env = read $ capitalize $ environment args

    config <- loadConfig env

    return $ if port args /= 0
                then config { appPort = port args }
                else config

    where
        capitalize [] = []
        capitalize (x:xs) = toUpper x : map toLower xs

-- | Load your development config (when using @'DefaultEnv'@)
loadDevelopmentConfig :: IO (AppConfig DefaultEnv)
loadDevelopmentConfig = loadConfig Development
