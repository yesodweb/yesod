{-# LANGUAGE CPP, TemplateHaskell #-}

import Scaffolding.Scaffolder
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.Process (rawSystem)
import Yesod.Core(yesodVersion)

import Options
import Types

import Build (touch)
import Devel (devel)

defineOptions "NoOptions" (return ())

defineOptions "DevelOptions" $ do
  mkOptNoApi    "develOptNoApi"

defineOptions "MainOptions" $ do
  mkOptCabalDev "optCabalDev"

type InitOptions      = NoOptions
type ConfigureOptions = NoOptions
type BuildOptions     = NoOptions
type TouchOptions     = NoOptions
type VersionOptions   = NoOptions

cabalCommand :: MainOptions -> String
cabalCommand mopt
  | optCabalDev mopt = "cabal-dev"
  | otherwise        = "cabal"

main = runSubcommand
      [ subcommand "init"      cmdInit
      , subcommand "configure" cmdConfigure
#ifndef WINDOWS
      , subcommand "build"     cmdBuild
      , subcommand "touch"     cmdTouch
#endif
      , subcommand "devel"     cmdDevel
      , subcommand "version"   cmdVersion
      ]

cmdInit :: MainOptions -> InitOptions -> [String] -> IO ()
cmdInit _ _ _ = scaffold

cmdConfigure :: MainOptions -> ConfigureOptions -> [String] -> IO ()
cmdConfigure mopt opts args = exitWith =<< rawSystem (cabalCommand mopt) ("configure":args)

cmdBuild :: MainOptions -> BuildOptions -> [String] -> IO ()
cmdBuild mopt opts args = do
  touch
  exitWith =<< rawSystem (cabalCommand mopt) ("build":args)

cmdTouch :: MainOptions -> TouchOptions -> [String] -> IO ()
cmdTouch _ _ _ = touch

cmdDevel :: MainOptions -> DevelOptions -> [String] -> IO ()
cmdDevel mopt opts args = devel (optCabalDev mopt) args
    where
      forceCabal = develOptNoApi opts

cmdVersion :: MainOptions -> VersionOptions -> [String] -> IO ()
cmdVersion _ _ _ = putStrLn $ "yesod-core version: " ++ yesodVersion



