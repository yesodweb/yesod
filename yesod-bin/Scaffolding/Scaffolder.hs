{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Scaffolding.Scaffolder (scaffold, backendOptions) where

import           Control.Arrow         ((&&&))
import qualified Data.ByteString.Char8 as S
import           Data.Conduit          (yield, ($$), ($$+-))
import Control.Monad.Trans.Resource (runResourceT)
import           Data.FileEmbed        (embedFile)
import           Data.String           (fromString)
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as LT
import qualified Data.Text.Lazy.IO     as TLIO
import           Text.ProjectTemplate  (unpackTemplate, receiveFS)
import           System.IO
import           Text.Shakespeare.Text (renderTextUrl, textFile)
import           Network.HTTP.Conduit  (Request, withManager, http, parseUrl, responseBody)
import           Data.Maybe            (isJust, fromJust)
import           Data.List             (intercalate)
import           Distribution.Text     (simpleParse)
import           Distribution.Package  (PackageName)

prompt :: (String -> Maybe a) -> IO a
prompt f = do
    s <- getLine
    case f s of
        Just a -> return a
        Nothing -> do
            putStr "That was not a valid entry, please try again: "
            hFlush stdout
            prompt f

data Backend = Sqlite
             | Postgresql
             | PostgresqlFay
             | Mysql
             | MongoDB
             | Simple
             | Minimal
  deriving (Eq, Read, Show, Enum, Bounded)

puts :: LT.Text -> IO ()
puts s = TLIO.putStr (LT.init s) >> hFlush stdout

backends :: [Backend]
backends = [minBound .. maxBound]

backendOptions :: String
backendOptions = intercalate "/" (map inputBackend backends)

showBackend :: Backend -> String
showBackend Sqlite = "s"
showBackend Postgresql = "p"
showBackend PostgresqlFay = "pf"
showBackend Mysql = "mysql"
showBackend MongoDB = "mongo"
showBackend Simple = "simple"
showBackend Minimal = "mini"

inputBackend :: Backend -> String
inputBackend Sqlite = "sqlite"
inputBackend Postgresql = "postgresql"
inputBackend PostgresqlFay = "postgresql_fay"
inputBackend Mysql = "mysql"
inputBackend MongoDB = "mongo"
inputBackend Simple = "simple"
inputBackend Minimal = "mini"

readBackend :: (Backend -> String) -> String -> Maybe Backend
readBackend f s = lookup s $ map (f &&& id) backends

backendBS :: Backend -> S.ByteString
backendBS Sqlite = $(embedFile "hsfiles/sqlite.hsfiles")
backendBS Postgresql = $(embedFile "hsfiles/postgres.hsfiles")
backendBS PostgresqlFay = $(embedFile "hsfiles/postgres-fay.hsfiles")
backendBS Mysql = $(embedFile "hsfiles/mysql.hsfiles")
backendBS MongoDB = $(embedFile "hsfiles/mongo.hsfiles")
backendBS Simple = $(embedFile "hsfiles/simple.hsfiles")
backendBS Minimal = $(embedFile "hsfiles/minimal.hsfiles")

validPackageName :: String -> Bool
validPackageName s = isJust (simpleParse s :: Maybe PackageName) && s /= "test"

scaffold :: Bool         -- ^ bare directory instead of a new subdirectory?
         -> Maybe String -- ^ application name
         -> Maybe String -- ^ database
         -> IO ()
scaffold isBare appName database = do
    puts $ renderTextUrl undefined $(textFile "input/welcome.cg")
    project <- projectName appName
    ebackend <- projectDatabase database

    putStrLn "That's it! I'm creating your files now..."

    let sink = unpackTemplate
                (receiveFS $ if isBare then "." else fromString project)
                ( T.replace "PROJECTNAME" (T.pack project)
                . T.replace "PROJECTNAME_LOWER" (T.toLower $ T.pack project)
                )
    case ebackend of
        Left req -> withManager $ \m -> do
            res <- http req m
            responseBody res $$+- sink
        Right backend -> runResourceT $ yield (backendBS backend) $$ sink

    let projectnameReplacer = if isBare
                                then LT.replace "cd PROJECTNAME && " ""
                                else LT.replace "PROJECTNAME" (LT.pack project)

    TLIO.putStr $ projectnameReplacer $ renderTextUrl undefined $(textFile "input/done.cg")

projectDatabase :: Maybe String -- ^ database
                -> IO (Either Request Backend)
projectDatabase Nothing = do
    puts $ renderTextUrl undefined $(textFile "input/database.cg")
    ebackend' <- prompt $ \s -> if s == "url" then Just (Left ()) else fmap Right $ readBackend showBackend s
    case ebackend' of
        Left () -> do
            puts "Please enter the URL:  "
            fmap Left $ prompt parseUrl
        Right backend -> return $ Right backend
projectDatabase (Just db)
    | "url" == db = do
        puts "Please enter the URL:  "
        fmap Left $ prompt parseUrl
    | isJust parsedDB = return $ Right (fromJust parsedDB)
    | otherwise       = error "Invalid value for --database option."
  where
    parsedDB = readBackend inputBackend db

projectName :: Maybe String -- ^ application name
            -> IO String
projectName Nothing = do
    puts $ renderTextUrl undefined $(textFile "input/project_name.cg")
    prompt $ \s -> if validPackageName s then Just s else Nothing
projectName (Just name) | validPackageName name = return name
                        | otherwise             = error "Invalid value for --name option."
