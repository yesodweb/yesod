{-# LANGUAGE OverloadedStrings #-}
module Keter
    ( keter
    ) where

import Data.Yaml
import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import System.Environment (getEnvironment)
import System.Exit
import System.Process
import Control.Monad
import System.Directory hiding (findFiles)
import Data.Maybe (mapMaybe,isJust,maybeToList)
import Data.Monoid
import System.FilePath ((</>))
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
      -> Bool -- ^ no copy to?
      -> [String] -- ^ build args
      -> IO ()
keter cabal noBuild noCopyTo buildArgs = do
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

    env' <- getEnvironment
    cwd' <- getCurrentDirectory
    files <- getDirectoryContents "."
    project <-
        case mapMaybe (T.stripSuffix ".cabal" . T.pack) files of
            [x] -> return x
            [] -> error "No cabal file found"
            _ -> error "Too many cabal files found"

    let findFiles (Object v) =
            mapM_ go $ Map.toList v
          where
            go ("exec", String s) = tellFile s
            go ("extraFiles", Array a) = Fold.mapM_ tellExtra a
            go (_, v') = findFiles v'
            tellFile s = tell [collapse $ "config" </> T.unpack s]
            tellExtra (String s) = tellFile s
            tellExtra _          = error "extraFiles should be a flat array"
        findFiles (Array v) = Fold.mapM_ findFiles v
        findFiles _ = return ()
        bundleFiles = execWriter $ findFiles $ Object value

        collapse = T.unpack . T.intercalate "/" . collapse' . T.splitOn "/" . T.pack
        collapse' (_:"..":rest) = collapse' rest
        collapse' (".":xs) = collapse' xs
        collapse' (x:xs) = x : collapse' xs
        collapse' [] = []

    unless noBuild $ do
        stackQueryRunSuccess <- do
            eres <- try $ readProcessWithExitCode "stack" ["query"] "" :: IO (Either IOException (ExitCode, String, String))
            return $ either (\_ -> False) (\(ec, _, _) -> (ec == ExitSuccess)) eres

        let inStackExec = isJust $ lookup "STACK_EXE" env'
            mStackYaml = lookup "STACK_YAML" env'
            useStack = inStackExec || isJust mStackYaml || stackQueryRunSuccess

        if useStack
            then do let stackYaml = maybeToList $ fmap ("--stack-yaml="<>) mStackYaml
                        localBinPath = cwd' </> "dist/bin"
                    run "stack" $ stackYaml <> ["clean"]
                    createDirectoryIfMissing True localBinPath
                    run "stack"
                        (stackYaml
                         <> ["--local-bin-path",localBinPath,"build","--copy-bins"]
                         <> buildArgs)
            else do run cabal ["clean"]
                    run cabal ["configure"]
                    run cabal ("build" : buildArgs)

    _ <- try' $ removeDirectoryRecursive "static/tmp"

    archive <- Tar.pack "" $
        "config" : "static" : bundleFiles
    let fp = T.unpack project ++ ".keter"
    L.writeFile fp $ compress $ Tar.write archive

    unless noCopyTo $ case Map.lookup "copy-to" value of
        Just (String s) ->
            let baseArgs = [fp, T.unpack s] :: [String]

                scpArgs =
                    case parseMaybe (.: "copy-to-args") value of
                        Just as -> as ++ baseArgs
                        Nothing -> baseArgs

                args =
                    case parseMaybe (.: "copy-to-port") value of
                        Just i -> "-P" : show (i :: Int) : scpArgs
                        Nothing -> scpArgs

            in run "scp" args

        _ -> return ()
  where
    -- Test for alternative config file extension (yaml or yml).
    keterConfig = do
        let yml = "config/keter.yml"
        ymlExists <- doesFileExist yml
        return $ if ymlExists then yml else "config/keter.yaml"

try' :: IO a -> IO (Either SomeException a)
try' = try
