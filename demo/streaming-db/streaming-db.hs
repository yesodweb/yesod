{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Monad.Logger    (runNoLoggingT)
import           Data.Conduit            (awaitForever, runResourceT, ($=))
import           Data.Text               (Text)
import           Database.Persist.Sqlite (ConnectionPool, SqlPersist,
                                          SqliteConf (..), runMigration,
                                          runSqlPool)
import           Database.Persist.Store  (createPoolConfig)
import           Yesod.Core
import           Yesod.Persist

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Person
    name Text
|]

data App = App
    { appConfig :: SqliteConf
    , appPool   :: ConnectionPool
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB = defaultRunDB appConfig appPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appPool

getHomeR :: Handler TypedContent
getHomeR = do
    runDB $ do
        runMigration migrateAll
        deleteWhere ([] :: [Filter Person])
        insert_ $ Person "Charlie"
        insert_ $ Person "Alice"
        insert_ $ Person "Bob"
    respondSourceDB typePlain $ selectSource [] [Asc PersonName] $= awaitForever toBuilder
  where
    toBuilder (Entity _ (Person name)) = do
        sendChunkText name
        sendChunkText "\n"
        sendFlush

main :: IO ()
main = do
    let config = SqliteConf ":memory:" 1
    pool <- createPoolConfig config
    runNoLoggingT $ runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        deleteWhere ([] :: [Filter Person])
        insert_ $ Person "Charlie"
        insert_ $ Person "Alice"
        insert_ $ Person "Bob"
    warp 3000 App
        { appConfig = config
        , appPool = pool
        }
