#!/usr/bin/env runhaskell

> module Main where
> import Distribution.Simple
> import System.Process (readProcessWithExitCode)
> import System.Exit (ExitCode(..))
> import System.Directory (removeFile)
> import System.IO.Error (try)

> main :: IO ()
> main = defaultMainWithHooks simpleUserHooks'
>  where
>    simpleUserHooks' = simpleUserHooks
>      { postConf = postConf'
>      , postClean = postClean'
>      }
>
>    postConf' x configFlags desc y = do
>      hconf <- checkHeimKinit
>      writeFile "config.h" $ concat
>          [ "#ifndef CONFIG_H\n"
>          , "#define CONFIG_H\n"
>          , "\n"
>          , "/* Define to 1 if you have Heimdal Kerberos. */\n"
>          , hconf
>          , "\n\n"
>          , "#endif\n"
>          ]
>      let configFlags' = updateConfigFlags configFlags
>      postConf simpleUserHooks x configFlags' desc y
>      where
>        updateConfigFlags configFlags = configFlags
>
>    postClean' _ _ _ _ = do
>      try . removeFile $ "config.h"
>      return ()
>
> checkHeimKinit :: IO String
> checkHeimKinit = do
>    (e,_,_) <- readProcessWithExitCode "kinit" ["--version"] ""
>    if e == ExitSuccess then
>        return "#define HAVE_HEIMDAL 1"
>        else return "/* #undef HAVE_HEIMDAL */"
