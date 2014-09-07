{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Defines the core functionality of this package. This package is
-- distinguished from Yesod.Persist in that the latter additionally exports the
-- persistent modules themselves.
module Yesod.Persist.Core
    ( YesodPersist (..)
    , defaultRunDB
    , YesodPersistRunner (..)
    , defaultGetDBRunner
    , DBRunner (..)
    , runDBSource
    , respondSourceDB
    , YesodDB
    , get404
    , getBy404
    ) where

import Database.Persist
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import Yesod.Core
import Data.Conduit
import Blaze.ByteString.Builder (Builder)
import Data.Pool
import Control.Monad.Trans.Resource
import Control.Exception (throwIO)
import Yesod.Core.Types (HandlerContents (HCError))
import qualified Database.Persist.Sql as SQL

unSqlPersistT :: a -> a
unSqlPersistT = id

type YesodDB site = ReaderT (YesodPersistBackend site) (HandlerT site IO)

class Monad (YesodDB site) => YesodPersist site where
    type YesodPersistBackend site
    runDB :: YesodDB site a -> HandlerT site IO a

-- | Helper for creating 'runDB'.
--
-- Since 1.2.0
defaultRunDB :: PersistConfig c
             => (site -> c)
             -> (site -> PersistConfigPool c)
             -> PersistConfigBackend c (HandlerT site IO) a
             -> HandlerT site IO a
defaultRunDB getConfig getPool f = do
    master <- getYesod
    Database.Persist.runPool
        (getConfig master)
        f
        (getPool master)

-- |
--
-- Since 1.2.0
class YesodPersist site => YesodPersistRunner site where
    -- | This function differs from 'runDB' in that it returns a database
    -- runner function, as opposed to simply running a single action. This will
    -- usually mean that a connection is taken from a pool and then reused for
    -- each invocation. This can be useful for creating streaming responses;
    -- see 'runDBSource'.
    --
    -- It additionally returns a cleanup function to free the connection.  If
    -- your code finishes successfully, you /must/ call this cleanup to
    -- indicate changes should be committed. Otherwise, for SQL backends at
    -- least, a rollback will be used instead.
    --
    -- Since 1.2.0
    getDBRunner :: HandlerT site IO (DBRunner site, HandlerT site IO ())

newtype DBRunner site = DBRunner
    { runDBRunner :: forall a. YesodDB site a -> HandlerT site IO a
    }

-- | Helper for implementing 'getDBRunner'.
--
-- Since 1.2.0
defaultGetDBRunner :: YesodPersistBackend site ~ SQL.SqlBackend
                   => (site -> Pool SQL.Connection)
                   -> HandlerT site IO (DBRunner site, HandlerT site IO ())
defaultGetDBRunner getPool = do
    pool <- fmap getPool getYesod
    let withPrep conn f = f conn (SQL.connPrepare conn)
    (relKey, (conn, local)) <- allocate
        (do
            (conn, local) <- takeResource pool
            withPrep conn SQL.connBegin
            return (conn, local)
            )
        (\(conn, local) -> do
            withPrep conn SQL.connRollback
            destroyResource pool local conn)

    let cleanup = liftIO $ do
            withPrep conn SQL.connCommit
            putResource local conn
            _ <- unprotect relKey
            return ()

    return (DBRunner $ \x -> runReaderT (unSqlPersistT x) conn, cleanup)

-- | Like 'runDB', but transforms a @Source@. See 'respondSourceDB' for an
-- example, practical use case.
--
-- Since 1.2.0
runDBSource :: YesodPersistRunner site
            => Source (YesodDB site) a
            -> Source (HandlerT site IO) a
runDBSource src = do
    (dbrunner, cleanup) <- lift getDBRunner
    transPipe (runDBRunner dbrunner) src
    lift cleanup

-- | Extends 'respondSource' to create a streaming database response body.
respondSourceDB :: YesodPersistRunner site
                => ContentType
                -> Source (YesodDB site) (Flush Builder)
                -> HandlerT site IO TypedContent
respondSourceDB ctype = respondSource ctype . runDBSource

-- | Get the given entity by ID, or return a 404 not found if it doesn't exist.
get404 :: (MonadIO m, PersistStore (PersistEntityBackend val), PersistEntity val)
       => Key val
       -> ReaderT (PersistEntityBackend val) m val
get404 key = do
    mres <- get key
    case mres of
        Nothing -> notFound'
        Just res -> return res

-- | Get the given entity by unique key, or return a 404 not found if it doesn't
--   exist.
getBy404 :: (PersistUnique (PersistEntityBackend val), PersistEntity val, MonadIO m)
         => Unique val
         -> ReaderT (PersistEntityBackend val) m (Entity val)
getBy404 key = do
    mres <- getBy key
    case mres of
        Nothing -> notFound'
        Just res -> return res

-- | Should be equivalent to @lift . notFound@, but there's an apparent bug in
-- GHC 7.4.2 that leads to segfaults. This is a workaround.
notFound' :: MonadIO m => m a
notFound' = liftIO $ throwIO $ HCError NotFound
