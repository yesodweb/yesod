{-
  wrapper executable that captures arguments to ghc, ar or ld
-}

{-# LANGUAGE CPP #-}
module Main where

import System.Process (rawSystem, readProcess)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import System.Exit (exitWith)
import System.Directory (doesDirectoryExist)

#ifdef LDCMD
cmd = lookupGhcInfo "ld command" "ld"
outFile = "dist/ldargs.txt"
#else
#ifdef ARCMD
cmd = lookupGhcInfo "ar command" "ar"
outFile ="dist/arargs.txt"
#else
cmd = return "ghc"
outFile = "dist/ghcargs.txt"
#endif
#endif

lookupGhcInfo :: String -> String -> IO String
lookupGhcInfo xs d = fmap (fromMaybe d . lookup xs . read) (readProcess "ghc" ["--info"] "")

passthrough args = do
  c <- cmd
  rawSystem c args

main = do
  args <- getArgs
  e <- doesDirectoryExist "dist"
  when e $ writeFile outFile (show args ++ "\n")
  ex <- passthrough args
  exitWith ex

