{-# LANGUAGE TemplateHaskell #-}
import CodeGen
import System.IO
import System.Directory
import qualified Data.ByteString.Char8 as S
import Language.Haskell.TH.Syntax

main :: IO ()
main = do
    putStr $(codegen "welcome")
    hFlush stdout
    name <- getLine

    putStr $(codegen "project-name")
    hFlush stdout
    project <- getLine

    putStr $(codegen "dir-name")
    hFlush stdout
    dirRaw <- getLine
    let dir = if null dirRaw then project else dirRaw

    putStr $(codegen "site-arg")
    hFlush stdout
    sitearg <- getLine

    putStr $(codegen "database")
    hFlush stdout
    backendS <- getLine
    let pconn1 = $(codegen "pconn1")
    let pconn2 = $(codegen "pconn2")
    let (lower, upper, connstr1, connstr2) =
            case backendS of
                "s" -> ("sqlite", "Sqlite", "debug.db3", "production.db3")
                "p" -> ("postgresql", "Postgresql", pconn1, pconn2)
                _ -> error $ "Invalid backend: " ++ backendS

    putStrLn "That's it! I'm creating your files now..."

    let writeFile' fp s = do
            putStrLn $ "Generating " ++ fp
            writeFile (dir ++ '/' : fp) s
        mkDir fp = createDirectoryIfMissing True $ dir ++ '/' : fp

    mkDir "Handler"
    mkDir "hamlet"
    mkDir "cassius"
    mkDir "julius"

    writeFile' "simple-server.hs" $(codegen "simple-server_hs")
    writeFile' "fastcgi.hs" $(codegen "fastcgi_hs")
    writeFile' "devel-server.hs" $(codegen "devel-server_hs")
    writeFile' (project ++ ".cabal") $(codegen "cabal")
    writeFile' "LICENSE" $(codegen "LICENSE")
    writeFile' (sitearg ++ ".hs") $(codegen "sitearg_hs")
    writeFile' "Controller.hs" $(codegen "Controller_hs")
    writeFile' "Handler/Root.hs" $(codegen "Root_hs")
    writeFile' "Model.hs" $(codegen "Model_hs")
    writeFile' "Settings.hs" $(codegen "Settings_hs")
    writeFile' "cassius/default-layout.cassius"
        $(codegen "default-layout_cassius")
    writeFile' "hamlet/default-layout.hamlet"
        $(codegen "default-layout_hamlet")
    writeFile' "hamlet/homepage.hamlet" $(codegen "homepage_hamlet")
    writeFile' "cassius/homepage.cassius" $(codegen "homepage_cassius")
    writeFile' "julius/homepage.julius" $(codegen "homepage_julius")

    S.writeFile (dir ++ "/favicon.ico")
        $(runIO (S.readFile "scaffold/favicon_ico.cg") >>= \bs -> do
            pack <- [|S.pack|]
            return $ pack `AppE` LitE (StringL $ S.unpack bs))
