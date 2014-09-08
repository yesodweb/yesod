{-
  wrapper executable that captures arguments to ghc, ar or ld
-}

{-# LANGUAGE CPP #-}
module Main where

import           Control.Monad                     (when)
import           Data.Maybe                        (fromMaybe)

import           Distribution.Compiler             (CompilerFlavor (..))
import           Distribution.Simple.Configure     (configCompiler)
import           Distribution.Simple.Program       (arProgram,
                                                    defaultProgramConfiguration,
                                                    ghcProgram, ldProgram,
                                                    programPath)
import           Distribution.Simple.Program.Db    (configureAllKnownPrograms,
                                                    lookupProgram)
import           Distribution.Simple.Program.Types (Program (..))
import           Distribution.Verbosity            (silent)

import           System.Directory                  (doesDirectoryExist)
import           System.Environment                (getArgs)
import           System.Exit                       (ExitCode (..), exitWith)
import           System.IO                         (hPutStrLn, stderr)
import           System.Process                    (rawSystem, readProcess)


#ifdef LDCMD
cmd :: Program
cmd = ldProgram
outFile = "yesod-devel/ldargs.txt"
#else
#ifdef ARCMD
cmd :: Program
cmd = arProgram
outFile ="yesod-devel/arargs.txt"
#else
cmd :: Program
cmd = ghcProgram
outFile = "yesod-devel/ghcargs.txt"
#endif
#endif

runProgram :: Program -> [String] -> IO ExitCode
runProgram pgm args = do
  (comp, pgmc) <- configCompiler (Just GHC) Nothing Nothing defaultProgramConfiguration silent
  pgmc' <- configureAllKnownPrograms silent pgmc
  case lookupProgram pgm pgmc' of
    Nothing -> do
      hPutStrLn stderr ("cannot find program '" ++ programName pgm ++ "'")
      return (ExitFailure 1)
    Just p -> rawSystem (programPath p) args

main :: IO ()
main = do
  args <- getArgs
  e <- doesDirectoryExist "yesod-devel"
  when e $ writeFile outFile (show args ++ "\n")
  ex <- runProgram cmd args
  exitWith ex
