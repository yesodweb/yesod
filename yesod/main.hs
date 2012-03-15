{-# LANGUAGE CPP #-}

import Scaffolding.Scaffolder
import System.Environment (getArgs)
import System.Exit (exitWith)
import System.Process (rawSystem)
import Yesod.Core(yesodVersion)

#ifndef WINDOWS
import Build (touch)
#endif
import Devel (devel)

windowsWarning :: String
#ifdef WINDOWS
windowsWarning = "\n                    (does not work on Windows)"
#else
windowsWarning = ""
#endif

main :: IO ()
main = do
    args' <- getArgs
    let (isDev, args) =
            case args' of
                "--dev":rest -> (True, rest)
                _ -> (False, args')
    let cmd = if isDev then "cabal-dev" else "cabal"
#ifndef WINDOWS
    let build rest = rawSystem cmd $ "build":rest
#endif
    case args of
        ["init"] -> scaffold
#ifndef WINDOWS
        "build":rest -> touch >> build rest >>= exitWith
        ["touch"] -> touch
#endif
        "devel":rest -> devel isDev rest
        ["version"] -> putStrLn $ "yesod-core version:" ++ yesodVersion
        "configure":rest -> rawSystem cmd ("configure":rest) >>= exitWith
        _ -> do
            putStrLn "Usage: yesod <command>"
            putStrLn "Available commands:"
            putStrLn "    init         Scaffold a new site"
            putStrLn "    configure    Configure a project for building"
            putStrLn $ "    build        Build project (performs TH dependency analysis)"
                ++ windowsWarning
            putStrLn $ "    touch        Touch any files with altered TH dependencies but do not build"
                ++ windowsWarning
            putStrLn "    devel        Run project with the devel server"
            putStrLn "                    use --dev devel to build with cabal-dev"
            putStrLn "    version      Print the version of Yesod"

