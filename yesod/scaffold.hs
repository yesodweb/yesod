{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
import CodeGen
import System.IO
import System.Directory
import qualified Data.ByteString.Char8 as S
import Language.Haskell.TH.Syntax
import Data.Time (getCurrentTime, utctDay, toGregorian)
import Data.Char (toLower)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Control.Monad (unless)
import System.Environment (getArgs)
import System.Exit (exitWith)

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
    let build rest = rawSystem cmd $ "build":rest
    case args of
        ["init"] -> scaffold
        "build":rest -> touch >> build rest >>= exitWith
        ["touch"] -> touch
        ["devel"] -> devel cabal
        "configure":rest -> rawSystem cmd ("configure":rest) >>= exitWith
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

data Backend = Sqlite | Postgresql | MongoDB | Tiny
  deriving (Eq, Read, Show, Enum, Bounded)

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

    putStrLn "That's it! I'm creating your files now..."

    let withConnectionPool = case backend of
          Sqlite     -> $(codegen $ "sqliteConnPool")
          Postgresql -> $(codegen $ "postgresqlConnPool")
          MongoDB    -> $(codegen $ "mongoDBConnPool")
          Tiny       -> ""

        settingsTextImport = case backend of
          Postgresql -> "import Data.Text (Text, concat)"
          _          -> "import Data.Text"

        packages = if backend == MongoDB then ", mongoDB\n               , bson\n" else ""

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

    case backend of
      Sqlite     -> writeFile' ("config/" ++ backendLower ++ ".yml") $(codegen ("config/sqlite.yml"))
      Postgresql -> writeFile' ("config/" ++ backendLower ++ ".yml") $(codegen ("config/postgresql.yml"))
      MongoDB    -> writeFile' ("config/" ++ backendLower ++ ".yml") $(codegen ("config/mongoDB.yml"))
      Tiny       -> return ()

    let isTiny = backend == Tiny
        ifTiny a b = if isTiny then a else b

    writeFile' ("config/settings.yml") $(codegen "config/settings.yml")
    writeFile' ("main.hs") $(codegen "main.hs")
    writeFile' (project ++ ".cabal") $ ifTiny $(codegen "tiny/cabal") $(codegen "project.cabal")
    writeFile' ".ghci" $(codegen ".ghci")
    writeFile' "LICENSE" $(codegen "LICENSE")
    writeFile' (sitearg ++ ".hs") $ ifTiny $(codegen "tiny/sitearg.hs") $(codegen "sitearg.hs")
    writeFile' "Handler.hs" $ ifTiny $(codegen "tiny/Handler.hs") $(codegen "Handler.hs")
    writeFile' "Handler/Root.hs" $ ifTiny $(codegen "tiny/Handler/Root.hs") $(codegen "Handler/Root.hs")
    unless isTiny $ writeFile' "Model.hs" $(codegen "Model.hs")
    writeFile' "config/Settings.hs" $ ifTiny $(codegen "tiny/config/Settings.hs") $(codegen "config/Settings.hs")
    writeFile' "config/StaticFiles.hs" $(codegen "config/StaticFiles.hs")
    writeFile' "cassius/default-layout.cassius"
        $(codegen "cassius/default-layout.cassius")
    writeFile' "hamlet/default-layout.hamlet"
        $(codegen "hamlet/default-layout.hamlet")
    writeFile' "hamlet/boilerplate-layout.hamlet"
        $(codegen "hamlet/boilerplate-layout.hamlet")
    writeFile' "static/css/normalize.css"
        $(codegen "static/css/normalize.css")
    writeFile' "hamlet/homepage.hamlet" $ ifTiny $(codegen "tiny/hamlet/homepage.hamlet") $(codegen "hamlet/homepage.hamlet")
    writeFile' "config/routes" $ ifTiny $(codegen "tiny/config/routes") $(codegen "config/routes")
    writeFile' "cassius/homepage.cassius" $(codegen "cassius/homepage.cassius")
    writeFile' "julius/homepage.julius" $(codegen "julius/homepage.julius")
    unless isTiny $ writeFile' "config/models" $(codegen "config/models")
  
    S.writeFile (dir ++ "/config/favicon.ico")
        $(runIO (S.readFile "scaffold/config/favicon.ico.cg") >>= \bs -> do
            pack <- [|S.pack|]
            return $ pack `AppE` LitE (StringL $ S.unpack bs))
    
