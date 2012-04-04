{-
  wrapper executable that captures arguments to ghc, ar or ld
-}

{-# LANGUAGE CPP #-}
module Main where

import Control.Monad (when)
import Data.Maybe (fromMaybe)

import Distribution.Compiler (CompilerFlavor(..))
import Distribution.Simple.Configure (configCompiler)
import Distribution.Simple.Program (defaultProgramConfiguration, programPath, ghcProgram,
                                    ldProgram, arProgram)
import Distribution.Simple.Program.Db (lookupProgram, configureProgram)
import Distribution.Simple.Program.Types (Program(..))
import Distribution.Verbosity (silent)

import System.Directory (doesDirectoryExist)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)
import System.Process (rawSystem, readProcess)


#ifdef LDCMD
cmd :: Program
cmd = ldProgram
outFile = "dist/ldargs.txt"
#else
#ifdef ARCMD
cmd :: Program
cmd = arProgram
outFile ="dist/arargs.txt"
#else
cmd :: Program
cmd = ghcProgram
outFile = "dist/ghcargs.txt"
#endif
#endif

runProgram :: Program -> [String] -> IO ExitCode
runProgram pgm args = do
  (comp, pgmc) <- configCompiler (Just GHC) Nothing Nothing defaultProgramConfiguration silent
  pgmc' <- configureProgram silent pgm pgmc
  case lookupProgram pgm pgmc' of
    Nothing -> do
      hPutStrLn stderr ("cannot find program '" ++ programName pgm ++ "'")
      return (ExitFailure 1)
    Just p -> rawSystem (programPath p) args

main = do
  args <- getArgs
  e <- doesDirectoryExist "dist"
  when e $ writeFile outFile (show args ++ "\n")
  ex <- runProgram cmd args
  exitWith ex


