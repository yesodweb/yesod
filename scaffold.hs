{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
import CodeGen
import System.IO
import System.Directory
import qualified Data.ByteString.Char8 as S
import Language.Haskell.TH.Syntax
import Data.Time (getCurrentTime, utctDay, toGregorian)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Control.Monad (when, unless)
import System.Environment (getArgs)

import Scaffold.Build (touch)
import Scaffold.Devel (devel)

import System.Process (rawSystem)

qq :: String
#if __GLASGOW_HASKELL__ >= 700
qq = ""
#else
qq = "$"
#endif

prompt :: (String -> Bool) -> IO String
prompt f = do
    s <- getLine
    if f s
        then return s
        else do
            putStrLn "That was not a valid entry, please try again: "
            prompt f

main :: IO ()
main = do
    args' <- getArgs
    let (isDev, args) =
            case args' of
                "--dev":rest -> (True, rest)
                _ -> (False, args')
    let cmd = if isDev then "cabal-dev" else "cabal"
    let cabal rest = rawSystem cmd rest >> return ()
    let conf rest = cabal $ "configure":rest
    let build rest = cabal $ "build":rest
    case args of
        ["init"] -> scaffold
        "build":rest -> touch >> build rest
        ["touch"] -> touch
        ["devel"] -> devel cabal
        "configure":rest -> conf rest
        _ -> do
            putStrLn "Usage: yesod <command>"
            putStrLn "Available commands:"
            putStrLn "    init         Scaffold a new site"
            putStrLn "    configure    Configure a project for building"
            putStrLn "    build        Build project (performs TH dependency analysis)"
            putStrLn "    touch        Touch any files with altered TH dependencies but do not build"
            putStrLn "    devel        Run project with the devel server"

puts :: String -> IO ()
puts s = putStr s >> hFlush stdout

scaffold :: IO ()
scaffold = do
    puts $(codegenDir "input" "welcome")
    name <- getLine

    puts $(codegenDir "input" "project-name")
    let validPN c
            | 'A' <= c && c <= 'Z' = True
            | 'a' <= c && c <= 'z' = True
            | '0' <= c && c <= '9' = True
        validPN '-' = True
        validPN _ = False
    project <- prompt $ all validPN
    let dir = project

    puts $(codegenDir "input" "site-arg")
    let isUpperAZ c = 'A' <= c && c <= 'Z'
    sitearg <- prompt $ \s -> not (null s) && all validPN s && isUpperAZ (head s) && s /= "Main"

    puts $(codegenDir "input" "database")
    backendS <- prompt $ flip elem ["s", "p", "m"]
    let pconn1 = $(codegen "pconn1")
    let (backendLower, upper, connParams, toConnStr, importDB) =
            case backendS of
                "s" -> ("sqlite", "Sqlite", "        return database", "", "import Database.Persist.Sqlite\n")
                "p" -> ("postgresql", "Postgresql", pconn1, ">>= return . toConnStr", "import Database.Persist.Postgresql\n")
                "m" -> (error "lower", error "upper", error "connParams", error "toConnStr", error "importDB")
                _ -> error $ "Invalid backend: " ++ backendS

    putStrLn "That's it! I'm creating your files now..."

    let fst3 (x, _, _) = x
    year <- show . fst3 . toGregorian . utctDay <$> getCurrentTime

    let writeFile' fp s = do
            putStrLn $ "Generating " ++ fp
            L.writeFile (dir ++ '/' : fp) $ LT.encodeUtf8 $ LT.pack s
        mkDir fp = createDirectoryIfMissing True $ dir ++ '/' : fp

    mkDir "Handler"
    mkDir "hamlet"
    mkDir "cassius"
    mkDir "lucius"
    mkDir "julius"
    mkDir "static"
    mkDir "static/css"
    mkDir "static/js"
    mkDir "config"
    mkDir "Model"
    mkDir "deploy"
     
    writeFile' ("deploy/Procfile") $(codegen "deploy/Procfile")

    case backendS of
        "s" -> writeFile' ("config/" ++ backendLower ++ ".yml") $(codegen ("config/sqlite.yml"))
        "p" -> writeFile' ("config/" ++ backendLower ++ ".yml") $(codegen ("config/postgresql.yml"))
        "m" -> return ()
        _ -> error $ "Invalid backend: " ++ backendS

    writeFile' ("config/settings.yml") $(codegen "config/settings.yml")
    writeFile' (project ++ ".hs") $(codegen "project.hs")
    writeFile' (project ++ ".cabal") $ if backendS == "m" then $(codegen "mini/cabal") else $(codegen "cabal")
    writeFile' ".ghci" $(codegen ".ghci")
    writeFile' "LICENSE" $(codegen "LICENSE")
    writeFile' (sitearg ++ ".hs") $ if backendS == "m" then $(codegen "mini/sitearg.hs") else $(codegen "sitearg.hs")
    writeFile' "Controller.hs" $ if backendS == "m" then $(codegen "mini/Controller.hs") else $(codegen "Controller.hs")
    writeFile' "Handler/Root.hs" $ if backendS == "m" then $(codegen "mini/Handler/Root.hs") else $(codegen "Handler/Root.hs")
    when (backendS /= "m") $ writeFile' "Model.hs" $(codegen "Model.hs")
    writeFile' "config/Settings.hs" $ if backendS == "m" then $(codegen "mini/config/Settings.hs") else $(codegen "config/Settings.hs")
    writeFile' "config/StaticFiles.hs" $(codegen "config/StaticFiles.hs")
    writeFile' "cassius/default-layout.cassius"
        $(codegen "cassius/default-layout.cassius")
    writeFile' "hamlet/default-layout.hamlet"
        $(codegen "hamlet/default-layout.hamlet")
    writeFile' "hamlet/boilerplate-layout.hamlet"
        $(codegen "hamlet/boilerplate-layout.hamlet")
    writeFile' "static/css/html5boilerplate.css"
        $(codegen "static/css/html5boilerplate.css")
    writeFile' "hamlet/homepage.hamlet" $ if backendS == "m" then $(codegen "mini/hamlet/homepage.hamlet") else $(codegen "hamlet/homepage.hamlet")
    writeFile' "config/routes" $ if backendS == "m" then $(codegen "mini/config/routes") else $(codegen "config/routes")
    writeFile' "cassius/homepage.cassius" $(codegen "cassius/homepage.cassius")
    writeFile' "julius/homepage.julius" $(codegen "julius/homepage.julius")
    unless (backendS == "m") $ writeFile' "config/models" $(codegen "config/models")
  
    S.writeFile (dir ++ "/config/favicon.ico")
        $(runIO (S.readFile "scaffold/config/favicon.ico.cg") >>= \bs -> do
            pack <- [|S.pack|]
            return $ pack `AppE` LitE (StringL $ S.unpack bs))
    
