{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Scaffolding.Scaffolder (scaffold) where

import Scaffolding.CodeGen

import Language.Haskell.TH.Syntax
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString.Lazy as L
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as S
import Data.Time (getCurrentTime, utctDay, toGregorian)
import Data.Char (toLower)
import System.Directory
import System.IO

prompt :: (String -> Bool) -> IO String
prompt f = do
    s <- getLine
    if f s
        then return s
        else do
            putStr "That was not a valid entry, please try again: "
            hFlush stdout
            prompt f

data Backend = Sqlite | Postgresql | Mysql | MongoDB
  deriving (Eq, Read, Show, Enum, Bounded)

puts :: String -> IO ()
puts s = putStr (init s) >> hFlush stdout

backends :: [Backend]
backends = [minBound .. maxBound]


scaffold :: IO ()
scaffold = do
    puts $(codegenDir "input" "welcome")
    name <- prompt $ not . null

    puts $(codegenDir "input" "project-name")
    let validPN c
            | 'A' <= c && c <= 'Z' = True
            | 'a' <= c && c <= 'z' = True
            | '0' <= c && c <= '9' = True
        validPN '-' = True
        validPN _ = False
    project <- prompt $ \s -> all validPN s && not (null s)
    let dir = project

    let sitearg = "App"

    puts $(codegenDir "input" "database")
    
    backendC <- prompt $ flip elem $ words "s p mysql mongo t"
    let (backend, importGenericDB, dbMonad, importPersist, mkPersistSettings) =
            case backendC of
                "s" -> (Sqlite,     "GenericSql", "SqlPersist", "Sqlite", "sqlSettings")
                "p" -> (Postgresql, "GenericSql", "SqlPersist", "Postgresql", "sqlSettings")
                "mysql" -> (Mysql, "GenericSql", "SqlPersist", "MySQL", "sqlSettings")
                "mongo" -> (MongoDB,    "MongoDB", "Action", "MongoDB", "MkPersistSettings { mpsBackend = ConT ''Action }")
                _ -> error $ "Invalid backend: " ++ backendC
        (modelImports) = case backend of
          MongoDB -> "import Database.Persist." ++ importGenericDB ++ "\nimport Language.Haskell.TH.Syntax"
          Sqlite -> ""
          Postgresql -> ""
          Mysql -> ""

        uncapitalize s = toLower (head s) : tail s
        backendLower = uncapitalize $ show backend 
        upper = show backend

    let runMigration  =
          case backend of
            MongoDB -> ""
            _ -> "\n    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p"

    let importMigration =
          case backend of
            MongoDB -> ""
            _ -> "\nimport Database.Persist.GenericSql (runMigration)"

    let dbConfigFile =
          case backend of
            MongoDB -> "mongoDB"
            Sqlite -> "sqlite"
            Postgresql -> "postgresql"
            Mysql -> "mysql"

    let configPersist =
          case backend of
            MongoDB -> "MongoConf"
            Sqlite -> "SqliteConf"
            Postgresql -> "PostgresConf"
            Mysql -> "MySQLConf"

    putStrLn "That's it! I'm creating your files now..."

    let withConnectionPool = case backend of
          Sqlite     -> $(codegen "sqliteConnPool")
          Postgresql -> $(codegen "postgresqlConnPool")
          Mysql      -> ""
          MongoDB    -> $(codegen "mongoDBConnPool")

        packages =
          if backend == MongoDB
            then "                 , persistent-mongoDB >= 0.8   && < 0.9\n                 , mongoDB >= 1.1\n                 , bson >= 0.1.5\n"
            else "                 , persistent-" ++ backendLower ++ " >= 0.8 && < 0.9"

        monadControlVersion = "== 0.3.*"


    let fst3 (x, _, _) = x
    year <- show . fst3 . toGregorian . utctDay <$> getCurrentTime

    let changeFile fileFunc fp s = do
            putStrLn $ "Generating " ++ fp
            fileFunc (dir ++ '/' : fp) $ LT.encodeUtf8 $ LT.pack s
        mkDir fp = createDirectoryIfMissing True $ dir ++ '/' : fp
        writeFile' = changeFile L.writeFile
        appendFile' = changeFile L.appendFile

    mkDir "Handler"
    mkDir "templates"
    mkDir "static"
    mkDir "static/css"
    mkDir "static/js"
    mkDir "config"
    mkDir "Model"
    mkDir "deploy"
    mkDir "Settings"
    mkDir "messages"
     
    writeFile' "deploy/Procfile" $(codegen "deploy/Procfile")

    case backend of
      Sqlite     -> writeFile' ("config/" ++ backendLower ++ ".yml") $(codegen "config/sqlite.yml")
      Postgresql -> writeFile' ("config/" ++ backendLower ++ ".yml") $(codegen "config/postgresql.yml")
      MongoDB    -> writeFile' ("config/" ++ backendLower ++ ".yml") $(codegen "config/mongoDB.yml")
      Mysql      -> writeFile' ("config/" ++ backendLower ++ ".yml") $(codegen "config/mysql.yml")

    writeFile' "config/settings.yml" $(codegen "config/settings.yml")
    writeFile' "main.hs" $(codegen "main.hs")
    writeFile' "devel.hs" $(codegen "devel.hs")
    writeFile' (project ++ ".cabal") $(codegen "project.cabal")

    writeFile' ".ghci" $(codegen ".ghci")
    writeFile' "LICENSE" $(codegen "LICENSE")
    writeFile' "Foundation.hs" $(codegen "Foundation.hs")
    writeFile' "Import.hs" $(codegen "Import.hs")
    writeFile' "Application.hs" $(codegen "Application.hs")
    writeFile' "Handler/Home.hs" $(codegen "Handler/Home.hs")
    writeFile' "Model.hs" $(codegen "Model.hs")
    writeFile' "Settings.hs" $(codegen "Settings.hs")
    writeFile' "Settings/StaticFiles.hs" $(codegen "Settings/StaticFiles.hs")
    writeFile' "Settings/Development.hs" $(codegen "Settings/Development.hs")
    writeFile' "static/css/bootstrap.css"
        $(codegen "static/css/bootstrap.css")
    writeFile' "templates/default-layout.hamlet"
        $(codegen "templates/default-layout.hamlet")
    writeFile' "templates/default-layout-wrapper.hamlet"
        $(codegen "templates/default-layout-wrapper.hamlet")
    writeFile' "templates/normalize.lucius"
        $(codegen "templates/normalize.lucius")
    writeFile' "templates/homepage.hamlet"
        $(codegen "templates/homepage.hamlet")
    writeFile' "config/routes" $(codegen "config/routes")
    writeFile' "templates/homepage.lucius"
        $(codegen "templates/homepage.lucius")
    writeFile' "templates/homepage.julius"
        $(codegen "templates/homepage.julius")
    writeFile' "config/models" $(codegen "config/models")
    writeFile' "messages/en.msg" $(codegen "messages/en.msg")

    mkDir "tests"
    writeFile' "tests/main.hs" $(codegen "tests/main.hs")
    writeFile' "tests/HomeTest.hs" $(codegen "tests/HomeTest.hs")

    S.writeFile (dir ++ "/config/favicon.ico")
        $(runIO (S.readFile "scaffold/config/favicon.ico.cg") >>= \bs -> do
            pack <- [|S.pack|]
            return $ pack `AppE` LitE (StringL $ S.unpack bs))

    S.writeFile (dir ++ "/config/robots.txt")
        $(runIO (S.readFile "scaffold/config/robots.txt.cg") >>= \bs ->
            [|S.pack $(return $ LitE $ StringL $ S.unpack bs)|])
    
    putStr $(codegenDir "input" "done")
