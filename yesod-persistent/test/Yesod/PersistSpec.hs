{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
module Yesod.PersistSpec where

import Test.Hspec
import Database.Persist.Sqlite
import Network.Wai.Test
import Yesod.Core
import Data.Conduit
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Yesod.Persist
import Data.Text (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name Text
    UniquePerson name
|]

data App = App
    { appConfig :: SqliteConf
    , appPool   :: ConnectionPool
    }

mkYesod "App" [parseRoutes|
/ HomeR GET
/ins InsertR GET
|]

instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
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
        yield $ Chunk $ fromText name
        yield $ Chunk $ fromText "\n"
        yield Flush

getInsertR :: Handler ()
getInsertR = runDB $ insert400_ $ Person "Alice"

test :: String -> Session () -> Spec
test name session = it name $ do
    let config = SqliteConf ":memory:" 1
    pool <- createPoolConfig config
    app <- toWaiApp $ App config pool
    runSession session app

spec :: Spec
spec = do
    test "streaming" $ do
        sres <- request defaultRequest
        assertBody "Alice\nBob\nCharlie\n" sres
        assertStatus 200 sres
    test "insert400" $ do
        sres <- request defaultRequest
        assertStatus 200 sres
        sres' <- request $ defaultRequest `setPath` "/ins"
        assertStatus 400 sres'
