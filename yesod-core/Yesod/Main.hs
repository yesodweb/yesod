{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Yesod.Main (defaultMain) where

import Yesod.Logger (Logger, makeLogger)
import Yesod.Settings (AppEnvironment(..), AppConfig(..), loadConfig)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import System.Console.CmdArgs hiding (args)
import Data.Char (toUpper, toLower)

defaultMain :: (AppConfig -> Logger -> (Application -> IO ()) -> IO ()) -> IO ()
defaultMain withSite = do
    logger <- makeLogger
    args   <- cmdArgs argConfig
    env    <- getAppEnv args
    config <- loadConfig env

    let c = if port args /= 0 
            then config { appPort = port args }
            else config

    withSite c logger $ run (appPort c)

data ArgConfig = ArgConfig
    { environment :: String
    , port        :: Int
    } deriving (Show, Data, Typeable)

argConfig :: ArgConfig
argConfig = ArgConfig
    { environment = def 
        &= help ("application environment, one of: " ++ (foldl1 (\a b -> a ++ ", " ++ b) environments))
        &= typ   "ENVIRONMENT"
    , port = def
        &= help "the port to listen on"
        &= typ  "PORT"
    }

getAppEnv :: ArgConfig -> IO AppEnvironment
getAppEnv cfg = do
    let e = if environment cfg /= ""
            then environment cfg
            else "development"
    return $ read $ capitalize e

    where
        capitalize [] = []
        capitalize (x:xs) = toUpper x : map toLower xs

environments :: [String]
environments = map ((map toLower) . show) ([minBound..maxBound] :: [AppEnvironment])
