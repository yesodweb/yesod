{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Devel
    ( devel
    ) where


import qualified Distribution.Simple.Utils as D
import qualified Distribution.Verbosity as D
import qualified Distribution.Package as D
import qualified Distribution.PackageDescription.Parse as D
import qualified Distribution.PackageDescription as D

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad (when, forever)

import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           System.Directory (doesFileExist, removeFile,
                                                getDirectoryContents)
import           System.Exit (exitFailure)
import           System.Posix.Types (EpochTime)
import           System.PosixCompat.Files (modificationTime, getFileStatus)
import           System.Process (runCommand, terminateProcess, 
                                           waitForProcess, rawSystem)

import Text.Shakespeare.Text (st)

import Build (getDeps, copySources, copyDeps, findHaskellFiles)

devel :: Bool -> IO ()
devel isDevel = do
    e <- doesFileExist "dist/devel-flag"
    when e $ removeFile "dist/devel-flag"

    cabal <- D.findPackageDesc "."
    gpd   <- D.readPackageDescription D.normal cabal
    let pid = (D.package . D.packageDescription) gpd

    checkCabalFile gpd

    copySources
    _ <- if isDevel
      then rawSystem "cabal-dev" ["configure", "--cabal-install-arg=-fdevel"]
      else rawSystem "cabal"     ["configure", "-fdevel"]

    T.writeFile "dist/devel.hs" (develFile pid)

    mainLoop isDevel


mainLoop :: Bool -> IO ()
mainLoop isDevel = forever $ do
   putStrLn "Rebuilding app"

   deps <- getDeps
   copyDeps deps

   list <- getFileList
   _ <- if isDevel
     then rawSystem "cabal"     ["build"]
     else rawSystem "cabal-dev" ["build"]

   putStrLn "Starting development server..."
   pkg <- pkgConfigs isDevel
   ph <- runCommand $ concat ["runghc ", pkg, " dist/devel.hs"]
   watchForChanges list
   putStrLn "Stopping development server..."
   _ <- forkIO $ do
     writeFile "dist/devel-flag" ""
     threadDelay 1000000
     -- fixme, check whether process is still alive?
     putStrLn "Terminating external process"
     terminateProcess ph
   ec <- waitForProcess ph
   putStrLn $ "Exit code: " ++ show ec

pkgConfigs :: Bool -> IO String
pkgConfigs isDev
  | isDev = do
      devContents <- getDirectoryContents "cabal-dev"
      let confs = filter isConfig devContents
      return . unwords $ inplacePkg :
             map ("-package-confcabal-dev/"++) confs
  | otherwise = return inplacePkg
  where
    inplacePkg = "-package-confdist/package.conf.inplace"
    isConfig pkg = "packages-" `L.isPrefixOf` pkg &&
                   ".conf"     `L.isSuffixOf` pkg

type FileList = Map.Map FilePath EpochTime

getFileList :: IO FileList
getFileList = do
    files <- findHaskellFiles "."
    deps <- getDeps
    let files' = files ++ map fst (Map.toList deps)
    fmap Map.fromList $ flip mapM files' $ \f -> do
        fs <- getFileStatus f
        return (f, modificationTime fs)

watchForChanges :: FileList -> IO () --  ThreadId -> IO ()
watchForChanges list = do
    newList <- getFileList
    if list /= newList
      then return ()
      else threadDelay 1000000 >> watchForChanges list

showPkgName :: D.PackageId -> String
showPkgName = (\(D.PackageName n) -> n) . D.pkgName

develFile :: D.PackageId -> T.Text
develFile pid = [st|
{-# LANGUAGE PackageImports #-}
import "#{showPkgName pid}" Application (withDevelAppPort)
import Data.Dynamic (fromDynamic)
import Network.Wai.Handler.Warp (run)
import Data.Maybe (fromJust)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  putStrLn "Starting app"
  wdap <- (return . fromJust . fromDynamic) withDevelAppPort
  forkIO . wdap $ \(port, app) -> run port app
  loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "dist/devel-flag"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = do
  removeFile "dist/devel-flag"
  putStrLn "Terminating server"
  exitSuccess
|]

{-
  check whether cabal file from old scaffold needs to be updated
  should be removed after 1.0 release?
-}
checkCabalFile :: D.GenericPackageDescription -> IO ()
checkCabalFile gpd = case D.condLibrary gpd of
    Nothing -> do
      putStrLn "Error: incorrect cabal file, no library"
      exitFailure
    Just ct ->
      case lookupDevelLib ct of
        Nothing   -> do
          putStrLn "Error: no library configuration for -fdevel"
          exitFailure
        Just dLib ->
         case (D.hsSourceDirs . D.libBuildInfo) dLib of
           ["dist/src-devel"]  -> return ()
           _                   ->
             T.putStrLn upgradeMessage >> exitFailure

lookupDevelLib :: D.CondTree D.ConfVar c a -> Maybe a
lookupDevelLib ct = listToMaybe . map (\(_,x,_) -> D.condTreeData x) .
                    filter isDevelLib . D.condTreeComponents  $ ct
  where
    isDevelLib ((D.Var (D.Flag (D.FlagName "devel"))), _, _) = True
    isDevelLib _                                             = False

upgradeMessage :: T.Text
upgradeMessage = [st|
Your cabal file needs to be updated for this version of yesod devel.
Find the lines:
library
    if flag(devel)
        Buildable: True
    else
        Buildable: False

    if os(windows)
        cpp-options: -DWINDOWS

    hs-source-dirs: .

And replace them with:
library
    if flag(devel)
        Buildable: True
        hs-source-dirs: dist/src-devel
    else
        Buildable: False
        hs-source-dirs: .

    if os(windows)
        cpp-options: -DWINDOWS
|]

