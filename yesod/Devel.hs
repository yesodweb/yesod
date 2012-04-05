{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP                 #-}
module Devel
    ( devel
    ) where


import qualified Distribution.Simple.Utils as D
import qualified Distribution.Verbosity as D
import qualified Distribution.PackageDescription.Parse as D
import qualified Distribution.PackageDescription as D
import qualified Distribution.ModuleName as D
import qualified Distribution.Simple.Configure as D
import qualified Distribution.Simple.Program as D
import qualified Distribution.Compiler as D

import           Control.Applicative ((<$>), (<*>))
import           Control.Concurrent (forkIO, threadDelay)
import qualified Control.Exception as Ex
import           Control.Monad (forever, when, unless)

import           Data.Char (isUpper, isNumber)
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set

import           System.Directory
import           System.Exit (exitFailure, exitSuccess, ExitCode (..))
import           System.FilePath (splitDirectories, dropExtension, takeExtension)
import           System.Posix.Types (EpochTime)
import           System.PosixCompat.Files (modificationTime, getFileStatus)
import           System.Process (createProcess, proc, terminateProcess, readProcess, ProcessHandle,
                                       getProcessExitCode,waitForProcess, rawSystem, runInteractiveProcess)
import           System.IO (hClose, hIsEOF, hGetLine, stdout, stderr, hPutStrLn)

import           Build (recompDeps, getDeps, isNewerThan)
import           GhcBuild (getBuildFlags, buildPackage)

import qualified Config as GHC
import           SrcLoc (Located)

lockFile :: FilePath
lockFile = "dist/devel-terminate"

writeLock :: IO ()
writeLock = do
    createDirectoryIfMissing True "dist"
    writeFile lockFile ""

removeLock :: IO ()
removeLock = try_ (removeFile lockFile)

devel :: Bool -> Bool -> [String] -> IO ()
devel isCabalDev forceCabal passThroughArgs = do
    checkDevelFile
    writeLock

    putStrLn "Yesod devel server. Press ENTER to quit"
    _ <- forkIO $ do
      cabal <- D.findPackageDesc "."
      gpd   <- D.readPackageDescription D.normal cabal

      ldar <- lookupLdAr
      hsSourceDirs <- checkCabalFile gpd

      _<- rawSystem cmd args

      mainLoop hsSourceDirs cabal ldar

    _ <- getLine
    writeLock
    exitSuccess
  where
    cmd | isCabalDev = "cabal-dev"
        | otherwise  = "cabal"

    diffArgs | isCabalDev = [
              "--cabal-install-arg=--with-compiler=yesod-ghc-wrapper"
            , "--cabal-install-arg=--with-hc-pkg=ghc-pkg"
            , "--cabal-install-arg=--with-ld=yesod-ld-wrapper"
            , "--cabal-install-arg=--with-ar=yesod-ar-wrapper"
            , "--cabal-install-arg=-fdevel" -- legacy
            , "--cabal-install-arg=-flibrary-only"
            ]
             | otherwise  = [
              "--with-compiler=yesod-ghc-wrapper"
            , "--with-hc-pkg=ghc-pkg"
            , "--with-ld=yesod-ld-wrapper"
            , "--with-ar=yesod-ar-wrapper"
            , "-fdevel" -- legacy
            , "-flibrary-only"
            ]
    args = "configure":diffArgs ++ ["--disable-library-profiling" ]

    mainLoop :: [FilePath] -> FilePath -> (FilePath, FilePath) -> IO ()
    mainLoop hsSourceDirs cabal ldar = do
       ghcVer <- ghcVersion
       _ <- rebuildCabal cmd
       pkgArgs <- ghcPackageArgs isCabalDev ghcVer
       rebuild <- mkRebuild ghcVer cabal cmd forceCabal ldar
       let devArgs = pkgArgs ++ ["devel.hs"] ++ passThroughArgs
       forever $ do
           recompDeps hsSourceDirs

           list <- getFileList hsSourceDirs [cabal]
           success <- rebuild
           if not success
             then putStrLn "Build failure, pausing..."
             else do
                   removeLock
                   putStrLn $ "Starting development server: runghc " ++ L.unwords devArgs
                   (_,_,_,ph) <- createProcess $ proc "runghc" devArgs
                   watchTid <- forkIO . try_ $ do
                         watchForChanges hsSourceDirs [cabal] list
                         putStrLn "Stopping development server..."
                         writeLock
                         threadDelay 1000000
                         putStrLn "Terminating development server..."
                         terminateProcess ph
                   ec <- waitForProcess' ph
                   putStrLn $ "Exit code: " ++ show ec
                   Ex.throwTo watchTid (userError "process finished")
           watchForChanges hsSourceDirs [cabal] list

mkRebuild :: String -> FilePath -> String -> Bool -> (FilePath, FilePath) -> IO (IO Bool)
mkRebuild ghcVer cabalFile cabalCmd forceCabal (ldPath, arPath)
  | forceCabal = return (rebuildCabal cabalCmd)
  | GHC.cProjectVersion == ghcVer = do
      bf <- getBuildFlags
      return $ do
        n1 <- cabalFile `isNewerThan` "dist/ghcargs.txt"
        n2 <- cabalFile `isNewerThan` "dist/arargs.txt"
        n3 <- cabalFile `isNewerThan` "dist/ldargs.txt"
        if n1 || n2 || n3
          then rebuildCabal cabalCmd
          else rebuildGhc bf ldPath arPath
  | otherwise = return $ do
                  putStrLn "WARNING: yesod is compiled with a different ghc version, falling back to cabal"
                  rebuildCabal cabalCmd

rebuildGhc :: [Located String] -> FilePath -> FilePath -> IO Bool
rebuildGhc bf ld ar = do
  putStrLn "Rebuilding application... (GHC API)"
  buildPackage bf ld ar

rebuildCabal :: String -> IO Bool
rebuildCabal cmd = do
  putStrLn $ "Rebuilding application... (" ++ cmd ++ ")"
  exit <- rawSystemFilter cmd ["build"]
  return $ case exit of
             ExitSuccess -> True
             _           -> False

try_ :: forall a. IO a -> IO ()
try_ x = (Ex.try x :: IO (Either Ex.SomeException a)) >> return ()

type FileList = Map.Map FilePath EpochTime

getFileList :: [FilePath] -> [FilePath] -> IO FileList
getFileList hsSourceDirs extraFiles = do
    (files, deps) <- getDeps hsSourceDirs
    let files' = extraFiles ++ files ++ map fst (Map.toList deps)
    fmap Map.fromList $ flip mapM files' $ \f -> do
        efs <- Ex.try $ getFileStatus f
        return $ case efs of
            Left (_ :: Ex.SomeException) -> (f, 0)
            Right fs -> (f, modificationTime fs)

watchForChanges :: [FilePath] -> [FilePath] -> FileList -> IO ()
watchForChanges hsSourceDirs extraFiles list = do
    newList <- getFileList hsSourceDirs extraFiles
    if list /= newList
      then return ()
      else threadDelay 1000000 >> watchForChanges hsSourceDirs extraFiles list

checkDevelFile :: IO ()
checkDevelFile = do
  e <- doesFileExist "devel.hs"
  unless e $ failWith "file devel.hs not found"

checkCabalFile :: D.GenericPackageDescription -> IO [FilePath]
checkCabalFile gpd = case D.condLibrary gpd of
    Nothing -> failWith "incorrect cabal file, no library"
    Just ct ->
      case lookupDevelLib ct of
        Nothing   ->
          failWith "no development flag found in your configuration file. Expected a 'library-only' flag or the older 'devel' flag"
        Just dLib -> do
           let hsSourceDirs = D.hsSourceDirs . D.libBuildInfo $ dLib
           fl <- getFileList hsSourceDirs []
           let unlisted = checkFileList fl dLib
           unless (null unlisted) $ do
                putStrLn "WARNING: the following source files are not listed in exposed-modules or other-modules:"
                mapM_ putStrLn unlisted
           when (D.fromString "Application" `notElem` D.exposedModules dLib) $
                putStrLn "WARNING: no exposed module Application"
           return hsSourceDirs

failWith :: String -> IO a
failWith msg = do
    putStrLn $ "ERROR: " ++ msg
    exitFailure

checkFileList :: FileList -> D.Library -> [FilePath]
checkFileList fl lib = filter isUnlisted . filter isSrcFile $ sourceFiles
  where
    al = allModules lib
    -- a file is only a possible 'module file' if all path pieces start with a capital letter
    sourceFiles = filter isSrcFile . map fst . Map.toList $ fl
    isSrcFile file = let dirs = filter (/=".") $ splitDirectories file
                     in  all (isUpper . head) dirs && (takeExtension file `elem` [".hs", ".lhs"])
    isUnlisted file = not (toModuleName file `Set.member` al)
    toModuleName = L.intercalate "." . filter (/=".") . splitDirectories . dropExtension

allModules :: D.Library -> Set.Set String
allModules lib = Set.fromList $ map toString $ D.exposedModules lib ++ (D.otherModules . D.libBuildInfo) lib
    where
      toString = L.intercalate "." . D.components

ghcVersion :: IO String
ghcVersion = fmap getNumber $ readProcess "runghc" ["--numeric-version", "0"] []
    where
      getNumber = filter (\x -> isNumber x || x == '.')

ghcPackageArgs :: Bool -> String -> IO [String]
ghcPackageArgs isCabalDev ghcVer
  | isCabalDev = do
      r <- readProcess "cabal-dev" ["buildopts"] []
      let opts = L.lines r
      return $ "-hide-all-packages" : "-no-user-package-conf" : inplacePkg : cabaldevConf : pkgid opts : depPkgIds opts
  | otherwise = return [inplacePkg]
      where
        pkgid opts      = let (_,p) = head (selectOpts ["-package-name"] opts) in "-package-id" ++ p ++ "-inplace"
        depPkgIds opts  = map (uncurry (++)) (selectOpts ["-package-id"] opts)
        inplacePkg   = "-package-confdist/package.conf.inplace"
        cabaldevConf = "-package-confcabal-dev/packages-" ++ ghcVer ++ ".conf"
        selectOpts opts (x1:x2:xs)
           | x1 `elem` opts = (x1,x2):selectOpts opts xs
           | otherwise      = selectOpts opts (x2:xs)
        selectOpts _ _ = []

lookupDevelLib :: D.CondTree D.ConfVar c a -> Maybe a
lookupDevelLib ct | found     = Just (D.condTreeData ct)
                  | otherwise = Nothing
  where
    found = not . null . map (\(_,x,_) -> D.condTreeData x) .
            filter isDevelLib . D.condTreeComponents  $ ct
    isDevelLib (D.Var (D.Flag (D.FlagName f)), _, _) = f `elem` ["library-only", "devel"]
    isDevelLib _                                       = False

-- location of `ld' and `ar' programs
lookupLdAr :: IO (FilePath, FilePath)
lookupLdAr = do 
  mla <- lookupLdAr'
  case mla of
    Nothing -> failWith "Cannot determine location of `ar' or `ld' program"
    Just la -> return la

lookupLdAr' :: IO (Maybe (FilePath, FilePath))
lookupLdAr' = do
  (comp, pgmc) <- D.configCompiler (Just D.GHC) Nothing Nothing D.defaultProgramConfiguration D.silent
  pgmc' <- D.configureAllKnownPrograms D.silent pgmc
  return $ (,) <$> look D.ldProgram pgmc' <*> look D.arProgram pgmc'
     where
       look pgm pdb = fmap D.programPath (D.lookupProgram pgm pdb)

-- | Acts like @rawSystem@, but filters out lines from the output that we're not interested in seeing.
rawSystemFilter :: String -> [String] -> IO ExitCode
rawSystemFilter command args = do
    (inh, outh, errh, ph) <- runInteractiveProcess command args Nothing Nothing
    hClose inh
    let go handlein handleout = do
            isEof <- hIsEOF handlein
            if isEof
                then hClose handlein
                else do
                    line <- hGetLine handlein
                    unless ("Loading package " `L.isPrefixOf` line) $ hPutStrLn handleout line
                    go handlein handleout
    _ <- forkIO $ go outh stdout
    _ <- forkIO $ go errh stderr
    waitForProcess' ph

-- nonblocking version
waitForProcess' :: ProcessHandle -> IO ExitCode
waitForProcess' pid = go
  where
    go = do
      mec <- getProcessExitCode pid
      case mec of
        Just ec -> return ec
        Nothing -> threadDelay 100000 >> go

