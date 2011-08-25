import Scaffolding.Scaffolder
import System.Environment (getArgs)
import System.Exit (exitWith)

import Build (touch)
import Devel (devel)

import System.Process (rawSystem)

main :: IO ()
main = do
    args' <- getArgs
    let (isDev, args) =
            case args' of
                "--dev":rest -> (True, rest)
                _ -> (False, args')
    let cmd = if isDev then "cabal-dev" else "cabal"
    let cabal rest = rawSystem cmd rest >> return ()
    let build rest = rawSystem cmd $ "build":rest
    case args of
        ["init"] -> scaffold
        "build":rest -> touch >> build rest >>= exitWith
        ["touch"] -> touch
        ["devel"] -> devel cabal
        ["version"] -> putStrLn "0.9"
        "configure":rest -> rawSystem cmd ("configure":rest) >>= exitWith
        _ -> do
            putStrLn "Usage: yesod <command>"
            putStrLn "Available commands:"
            putStrLn "    init         Scaffold a new site"
            putStrLn "    configure    Configure a project for building"
            putStrLn "    build        Build project (performs TH dependency analysis)"
            putStrLn "    touch        Touch any files with altered TH dependencies but do not build"
            putStrLn "    devel        Run project with the devel server"
            putStrLn "    version      Print the version of Yesod"
