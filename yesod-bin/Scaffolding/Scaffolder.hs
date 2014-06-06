{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Scaffolding.Scaffolder (scaffold) where

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
import Network.HTTP.Conduit (withManager, http, parseUrl, responseBody)
import           Data.List.Split       (splitOn)
import           Data.Char             (isDigit)

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

readBackend :: String -> Maybe Backend
readBackend s = lookup s $ map (showBackend &&& id) backends

backendBS :: Backend -> S.ByteString
backendBS Sqlite = $(embedFile "hsfiles/sqlite.hsfiles")
backendBS Postgresql = $(embedFile "hsfiles/postgres.hsfiles")
backendBS PostgresqlFay = $(embedFile "hsfiles/postgres-fay.hsfiles")
backendBS Mysql = $(embedFile "hsfiles/mysql.hsfiles")
backendBS MongoDB = $(embedFile "hsfiles/mongo.hsfiles")
backendBS Simple = $(embedFile "hsfiles/simple.hsfiles")

-- | Is the character valid for a project name?
validPN :: Char -> Bool
validPN c
    | 'A' <= c && c <= 'Z' = True
    | 'a' <= c && c <= 'z' = True
    | '0' <= c && c <= '9' = True
validPN '-' = True
validPN _ = False

-- | Cabal separates packages with a hyphen into words. A word can't consist of only digits
-- <http://www.haskell.org/ghc/docs/7.0.3/html/Cabal/packages.html Relevant Cabal Docs>
-- Fixes <https://github.com/yesodweb/yesod/issues/550 #550>
wordsHaveOneCharacter :: String -> Bool
wordsHaveOneCharacter s = not $ any (all isDigit) (splitOn "-" s)

scaffold :: Bool -- ^ bare directory instead of a new subdirectory?
         -> IO ()
scaffold isBare = do
    puts $ renderTextUrl undefined $(textFile "input/welcome.cg")
    project <- prompt $ \s ->
        if all validPN s && not (null s) && s /= "test" && wordsHaveOneCharacter s
            then Just s
            else Nothing
    let dir = project

    puts $ renderTextUrl undefined $(textFile "input/database.cg")

    ebackend' <- prompt $ \s -> if s == "url" then Just (Left ()) else fmap Right $ readBackend s

    ebackend <-
        case ebackend' of
            Left () -> do
                puts "Please enter the URL:  "
                fmap Left $ prompt parseUrl
            Right backend -> return $ Right backend

    putStrLn "That's it! I'm creating your files now..."

    let sink = unpackTemplate
                (receiveFS $ if isBare then "." else fromString project)
                (T.replace "PROJECTNAME" (T.pack project))
    case ebackend of
        Left req -> withManager $ \m -> do
            res <- http req m
            responseBody res $$+- sink
        Right backend -> runResourceT $ yield (backendBS backend) $$ sink

    TLIO.putStr $ LT.replace "PROJECTNAME" (LT.pack project) $ renderTextUrl undefined $(textFile "input/done.cg")
