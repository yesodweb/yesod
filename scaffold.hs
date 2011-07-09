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

import Scaffold.Build (build, touch)
import Scaffold.Devel (devel)

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
    args <- getArgs
    case args of
        ["init"] -> scaffold
        ["build"] -> build
        ["touch"] -> touch
        ["devel"] -> devel
        _ -> do
            putStrLn "Usage: yesod <command>"
            putStrLn "Available commands:"
            putStrLn "    init         Scaffold a new site"
            putStrLn "    build        Build project (performs TH dependency analysis)"
            putStrLn "    touch        Touch any files with altered TH dependencies but do not build"
            putStrLn "    devel        Run project with the devel server"

puts :: String -> IO ()
puts s = putStr s >> hFlush stdout

scaffold :: IO ()
scaffold = do
    puts $(codegen "welcome")
    name <- getLine

    puts $(codegen "project-name")
    let validPN c
            | 'A' <= c && c <= 'Z' = True
            | 'a' <= c && c <= 'z' = True
            | '0' <= c && c <= '9' = True
        validPN '-' = True
        validPN _ = False
    project <- prompt $ all validPN
    let dir = project

    puts $(codegen "site-arg")
    let isUpperAZ c = 'A' <= c && c <= 'Z'
    sitearg <- prompt $ \s -> not (null s) && all validPN s && isUpperAZ (head s) && s /= "Main"

    puts $(codegen "database")
    backendS <- prompt $ flip elem ["s", "p", "m"]
    let pconn1 = $(codegen "pconn1")
    let pconn2 = $(codegen "pconn2")
    let (lower, upper, connstr1, connstr2, importDB) =
            case backendS of
                "s" -> ("sqlite", "Sqlite", "debug.db3", "production.db3", "import Database.Persist.Sqlite\n")
                "p" -> ("postgresql", "Postgresql", pconn1, pconn2, "import Database.Persist.Postgresql\n")
                "m" -> ("FIXME lower", "FIXME upper", "FIXME connstr1", "FIXME connstr2", "")
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
    mkDir "config"
    mkDir "deploy"

    writeFile' ("deploy/Procfile") $(codegen "Procfile")
    writeFile' ("config/" ++ project ++ ".hs") $(codegen "test_hs")
    writeFile' (project ++ ".cabal") $ if backendS == "m" then $(codegen "mini-cabal") else $(codegen "cabal")
    writeFile' ".ghci" $(codegen "dotghci")
    writeFile' "LICENSE" $(codegen "LICENSE")
    writeFile' (sitearg ++ ".hs") $ if backendS == "m" then $(codegen "mini-sitearg_hs") else $(codegen "sitearg_hs")
    writeFile' "Controller.hs" $ if backendS == "m" then $(codegen "mini-Controller_hs") else $(codegen "Controller_hs")
    writeFile' "Handler/Root.hs" $ if backendS == "m" then $(codegen "mini-Root_hs") else $(codegen "Root_hs")
    when (backendS /= "m") $ writeFile' "Model.hs" $(codegen "Model_hs")
    writeFile' "config/Settings.hs" $ if backendS == "m" then $(codegen "mini-Settings_hs") else $(codegen "Settings_hs")
    writeFile' "config/StaticFiles.hs" $(codegen "StaticFiles_hs")
    writeFile' "cassius/default-layout.cassius"
        $(codegen "default-layout_cassius")
    writeFile' "hamlet/default-layout.hamlet"
        $(codegen "default-layout_hamlet")
    writeFile' "hamlet/boilerplate-layout.hamlet"
        $(codegen "boilerplate-layout_hamlet")
    writeFile' "static/css/html5boilerplate.css"
        $(codegen "boilerplate_css")
    writeFile' "hamlet/homepage.hamlet" $ if backendS == "m" then $(codegen "mini-homepage_hamlet") else $(codegen "homepage_hamlet")
    writeFile' "config/routes" $ if backendS == "m" then $(codegen "mini-routes") else $(codegen "routes")
    writeFile' "cassius/homepage.cassius" $(codegen "homepage_cassius")
    writeFile' "julius/homepage.julius" $(codegen "homepage_julius")
    unless (backendS == "m") $ writeFile' "config/models" $(codegen "entities")
  
    S.writeFile (dir ++ "/config/favicon.ico")
        $(runIO (S.readFile "scaffold/favicon_ico.cg") >>= \bs -> do
            pack <- [|S.pack|]
            return $ pack `AppE` LitE (StringL $ S.unpack bs))
    
