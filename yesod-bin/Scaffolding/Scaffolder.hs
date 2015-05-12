{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Scaffolding.Scaffolder (scaffold) where

import           Control.Arrow         ((&&&))
import           Control.Monad         (mfilter)
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
import Network.HTTP.Conduit (withManager, http, parseUrl, responseBody)
import           Data.Maybe            (isJust)
import           Distribution.Text     (simpleParse)
import           Distribution.Package  (PackageName)

prompt :: (String -> Maybe a) -> Maybe String -> IO a
prompt f (Just s) = accept f s
prompt f Nothing  = accept f =<< getLine

accept :: (String -> Maybe a) -> String -> IO a
accept parser s = do
    case parser s of
        Just a -> return a
        Nothing -> do
            putStr "That was not a valid entry, please try again: "
            hFlush stdout
            getLine >>= accept parser

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

showBackend :: Backend -> String
showBackend Sqlite = "s"
showBackend Postgresql = "p"
showBackend PostgresqlFay = "pf"
showBackend Mysql = "mysql"
showBackend MongoDB = "mongo"
showBackend Simple = "simple"
showBackend Minimal = "mini"

readBackend :: String -> Maybe Backend
readBackend s = lookup s $ map (showBackend &&& id) backends

backendBS :: Backend -> S.ByteString
backendBS Sqlite = $(embedFile "hsfiles/sqlite.hsfiles")
backendBS Postgresql = $(embedFile "hsfiles/postgres.hsfiles")
backendBS PostgresqlFay = $(embedFile "hsfiles/postgres-fay.hsfiles")
backendBS Mysql = $(embedFile "hsfiles/mysql.hsfiles")
backendBS MongoDB = $(embedFile "hsfiles/mongo.hsfiles")
backendBS Simple = $(embedFile "hsfiles/simple.hsfiles")
backendBS Minimal = $(embedFile "hsfiles/minimal.hsfiles")

validPackageName :: String -> Bool
validPackageName s =
    isJust (simpleParse s :: Maybe PackageName) && s /= "test"

parsePackageName :: String -> Maybe String
parsePackageName = mfilter validPackageName . Just

parseBackend :: String -> Maybe (Either () Backend)
parseBackend "url" = Just (Left ())
parseBackend s     = fmap Right $ readBackend s

scaffold :: Bool         -- ^ bare directory instead of a new subdirectory?
         -> Maybe String -- ^ project name
         -> Maybe String -- ^ backend
         -> IO ()
scaffold isBare projectName projectBackend = do
    puts $ renderTextUrl undefined $(textFile "input/welcome.cg")
    project <- prompt parsePackageName projectName

    puts $ renderTextUrl undefined $(textFile "input/database.cg")

    ebackend' <- prompt parseBackend projectBackend

    ebackend <-
        case ebackend' of
            Left () -> do
                puts "Please enter the URL:  "
                fmap Left $ prompt parseUrl Nothing
            Right backend -> return $ Right backend

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
