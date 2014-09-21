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
import qualified Data.Foldable as Fold
import Control.Monad.Trans.Writer (tell, execWriter)

run :: String -> [String] -> IO ()
run a b = do
    ec <- rawSystem a b
    unless (ec == ExitSuccess) $ exitWith ec

keter :: String -- ^ cabal command
      -> Bool -- ^ no build?
      -> IO ()
keter cabal noBuild = do
    ketercfg <- keterConfig
    mvalue <- decodeFile ketercfg
    value <-
        case mvalue of
            Nothing -> error "No config/keter.yaml found"
            Just (Object value) ->
                case Map.lookup "host" value of
                    Just (String s) | "<<" `T.isPrefixOf` s ->
                        error $ "Please set your hostname in " ++ ketercfg
                    _ ->
                        case Map.lookup "user-edited" value of
                            Just (Bool False) ->
                                error $ "Please edit your Keter config file at "
                                     ++ ketercfg
                            _ -> return value
            Just _ -> error $ ketercfg ++ " is not an object"

    files <- getDirectoryContents "."
    project <-
        case mapMaybe (T.stripSuffix ".cabal" . T.pack) files of
            [x] -> return x
            [] -> error "No cabal file found"
            _ -> error "Too many cabal files found"

    let findExecs (Object v) =
            mapM_ go $ Map.toList v
          where
            go ("exec", String s) = tell [F.collapse $ "config" F.</> F.fromText s]
            go (_, v') = findExecs v'
        findExecs (Array v) = Fold.mapM_ findExecs v
        findExecs _ = return ()
        execs = execWriter $ findExecs $ Object value

    unless noBuild $ do
        run cabal ["clean"]
        run cabal ["configure"]
        run cabal ["build"]

    _ <- try' $ F.removeTree "static/tmp"

    archive <- Tar.pack "" $ "config" : "static" : map F.encodeString execs
    let fp = T.unpack project ++ ".keter"
    L.writeFile fp $ compress $ Tar.write archive

    case Map.lookup "copy-to" value of
        Just (String s) ->
            case parseMaybe (.: "copy-to-port") value of
                Just i -> run "scp" ["-P" ++ show (i :: Int), fp, T.unpack s]
                Nothing -> run "scp" [fp, T.unpack s]
        _ -> return ()
  where
    -- Test for alternative config file extension (yaml or yml).
    keterConfig = do
        let yml = "config/keter.yml"
        ymlExists <- doesFileExist yml
        return $ if ymlExists then yml else "config/keter.yaml"

try' :: IO a -> IO (Either SomeException a)
try' = try
