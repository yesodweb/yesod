{-# LANGUAGE OverloadedStrings #-}
module Keter
    ( keter
    ) where

import Data.Yaml
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import System.Exit
import System.Process
import Control.Monad
import System.Directory
import Data.Maybe (mapMaybe)
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import qualified Codec.Archive.Tar as Tar
import Control.Exception
import qualified Data.ByteString.Lazy as L
import Codec.Compression.GZip (compress)

run :: String -> [String] -> IO ()
run a b = do
    ec <- rawSystem a b
    unless (ec == ExitSuccess) $ exitWith ec

keter :: String -- ^ cabal command
      -> Bool -- ^ no build?
      -> IO ()
keter cabal noBuild = do
    mvalue <- decodeFile "config/keter.yaml"
    value <-
        case mvalue of
            Nothing -> error "No config/keter.yaml found"
            Just (Object value) ->
                case Map.lookup "host" value of
                    Just (String s) | "<<" `T.isPrefixOf` s ->
                        error "Please set your hostname in config/keter.yaml"
                    _ -> return value
            Just _ -> error "config/keter.yaml is not an object"

    files <- getDirectoryContents "."
    project <-
        case mapMaybe (T.stripSuffix ".cabal" . T.pack) files of
            [x] -> return x
            [] -> error "No cabal file found"
            _ -> error "Too many cabal files found"

    exec <-
        case Map.lookup "exec" value of
            Just (String s) -> return $ F.collapse $ "config" F.</> F.fromText s
            _ -> error "exec not found in config/keter.yaml"

    unless noBuild $ do
        run cabal ["clean"]
        run cabal ["configure"]
        run cabal ["build"]

    _ <- try' $ F.removeTree "static/tmp"

    archive <- Tar.pack "" [F.encodeString exec, "config", "static"]
    let fp = T.unpack project ++ ".keter"
    L.writeFile fp $ compress $ Tar.write archive

    case Map.lookup "copy-to" value of
        Just (String s) ->
            case parseMaybe (.: "copy-to-port") value of
                Just i -> run "scp" ["-P" ++ show (i :: Int), fp, T.unpack s]
                Nothing -> run "scp" [fp, T.unpack s]
        _ -> return ()

try' :: IO a -> IO (Either SomeException a)
try' = try
