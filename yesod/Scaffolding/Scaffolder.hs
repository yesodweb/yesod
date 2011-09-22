{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Scaffolding.Scaffolder (scaffold) where

import Scaffolding.CodeGen

import Language.Haskell.TH.Syntax
import Control.Monad (unless)
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
            putStrLn "That was not a valid entry, please try again: "
            prompt f

qq :: String
#if __GLASGOW_HASKELL__ >= 700
qq = ""
#else
qq = "$"
#endif

data Backend = Sqlite | Postgresql | MongoDB | Tiny
  deriving (Eq, Read, Show, Enum, Bounded)

puts :: String -> IO ()
puts s = putStr s >> hFlush stdout

backends :: [Backend]
backends = [minBound .. maxBound]


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
    
    backendC <- prompt $ flip elem $ map (return . toLower . head . show) backends
    let (backend, importGenericDB, dbMonad, importPersist, mkPersistSettings) =
            case backendC of
                "s" -> (Sqlite,     "GenericSql", "SqlPersist", "Sqlite\n", "sqlSettings")
                "p" -> (Postgresql, "GenericSql", "SqlPersist", "Postgresql\n", "sqlSettings")
                "m" -> (MongoDB,    "MongoDB", "Action", "MongoDB\nimport Control.Applicative (Applicative)\n", "MkPersistSettings { mpsBackend = ConT ''Action }")
                "t" -> (Tiny, "","","",undefined)
                _ -> error $ "Invalid backend: " ++ backendC
        (modelImports) = case backend of
          MongoDB -> "import Database.Persist." ++ importGenericDB ++ "\nimport Language.Haskell.TH.Syntax"
          Sqlite -> ""
          Postgresql -> ""
          Tiny -> undefined

        uncapitalize s = toLower (head s) : tail s
        backendLower = uncapitalize $ show backend 
        upper = show backend

    let runMigration  =
          case backend of
            MongoDB -> ""
            _ -> "\n        runConnectionPool (runMigration migrateAll) p"

    putStrLn "That's it! I'm creating your files now..."

    let withConnectionPool = case backend of
          Sqlite     -> $(codegen $ "sqliteConnPool")
          Postgresql -> $(codegen $ "postgresqlConnPool")
          MongoDB    -> $(codegen $ "mongoDBConnPool")
          Tiny       -> ""

        packages =
          if backend == MongoDB
            then "                 , persistent-mongoDB >= 0.6.1 && < 0.7\n                 , mongoDB >= 1.1\n                 , bson >= 0.1.5\n"
            else "                 , persistent-" ++ backendLower ++ " >= 0.6 && < 0.7"


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
    mkDir "Settings"
     
    writeFile' ("deploy/Procfile") $(codegen "deploy/Procfile")

    case backend of
      Sqlite     -> writeFile' ("config/" ++ backendLower ++ ".yml") $(codegen ("config/sqlite.yml"))
      Postgresql -> writeFile' ("config/" ++ backendLower ++ ".yml") $(codegen ("config/postgresql.yml"))
      MongoDB    -> writeFile' ("config/" ++ backendLower ++ ".yml") $(codegen ("config/mongoDB.yml"))
      Tiny       -> return ()

    let isTiny = backend == Tiny
        ifTiny a b = if isTiny then a else b

    writeFile' ("config/settings.yml") $(codegen "config/settings.yml")
    writeFile' ("main.hs") $(codegen "main.hs")
    writeFile' (project ++ ".cabal") $ ifTiny $(codegen "tiny/project.cabal") $(codegen "project.cabal")
    writeFile' ".ghci" $(codegen ".ghci")
    writeFile' "LICENSE" $(codegen "LICENSE")
    writeFile' ("Foundation.hs") $ ifTiny $(codegen "tiny/Foundation.hs") $(codegen "Foundation.hs")
    writeFile' "Application.hs" $ ifTiny $(codegen "tiny/Application.hs") $(codegen "Application.hs")
    writeFile' "Handler/Root.hs" $(codegen "Handler/Root.hs")
    unless isTiny $ writeFile' "Model.hs" $(codegen "Model.hs")
    writeFile' "Settings.hs" $ ifTiny $(codegen "tiny/Settings.hs") $(codegen "Settings.hs")
    writeFile' "Settings/StaticFiles.hs" $(codegen "Settings/StaticFiles.hs")
    writeFile' "lucius/default-layout.lucius"
        $(codegen "lucius/default-layout.lucius")
    writeFile' "hamlet/default-layout.hamlet"
        $(codegen "hamlet/default-layout.hamlet")
    writeFile' "hamlet/boilerplate-layout.hamlet"
        $(codegen "hamlet/boilerplate-layout.hamlet")
    writeFile' "lucius/normalize.lucius"
        $(codegen "lucius/normalize.lucius")
    writeFile' "hamlet/homepage.hamlet" $(codegen "hamlet/homepage.hamlet")
    writeFile' "config/routes" $ ifTiny $(codegen "tiny/config/routes") $(codegen "config/routes")
    writeFile' "lucius/homepage.lucius" $(codegen "lucius/homepage.lucius")
    writeFile' "julius/homepage.julius" $(codegen "julius/homepage.julius")
    unless isTiny $ writeFile' "config/models" $(codegen "config/models")
  
    S.writeFile (dir ++ "/config/favicon.ico")
        $(runIO (S.readFile "scaffold/config/favicon.ico.cg") >>= \bs -> do
            pack <- [|S.pack|]
            return $ pack `AppE` LitE (StringL $ S.unpack bs))
    
    puts $(codegenDir "input" "done")
