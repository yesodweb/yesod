{-# LANGUAGE ConstraintKinds #-}
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
    , insert400
    , insert400_
    ) where

import Database.Persist
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import Data.Foldable (toList)
import Yesod.Core
import Data.Conduit
import Blaze.ByteString.Builder (Builder)
import Data.Pool
import Control.Monad.Trans.Resource
import Control.Exception (throwIO)
import Yesod.Core.Types (HandlerContents (HCError))
import qualified Database.Persist.Sql as SQL
#if MIN_VERSION_persistent(2,13,0)
import qualified Database.Persist.SqlBackend.Internal as SQL
#endif
#if MIN_VERSION_persistent(2,14,0)
import Database.Persist.Class.PersistEntity
#endif

unSqlPersistT :: a -> a
unSqlPersistT = id

type YesodDB site = ReaderT (YesodPersistBackend site) (HandlerFor site)

class Monad (YesodDB site) => YesodPersist site where
    type YesodPersistBackend site
    -- | Allows you to execute database actions within Yesod Handlers. For databases that support it, code inside the action will run as an atomic transaction.
    --
    --
    -- ==== __Example Usage__
    --
    -- > userId <- runDB $ do
    -- >   userId <- insert $ User "username" "email@example.com"
    -- >   insert_ $ UserPreferences userId True
    -- >   pure userId
    runDB :: YesodDB site a -> HandlerFor site a

-- | Helper for creating 'runDB'.
--
-- Since 1.2.0
defaultRunDB :: PersistConfig c
             => (site -> c)
             -> (site -> PersistConfigPool c)
             -> PersistConfigBackend c (HandlerFor site) a
             -> HandlerFor site a
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
    getDBRunner :: HandlerFor site (DBRunner site, HandlerFor site ())

newtype DBRunner site = DBRunner
    { runDBRunner :: forall a. YesodDB site a -> HandlerFor site a
    }

-- | Helper for implementing 'getDBRunner'.
--
-- Since 1.2.0
#if MIN_VERSION_persistent(2,5,0)
defaultGetDBRunner :: (SQL.IsSqlBackend backend, YesodPersistBackend site ~ backend)
                   => (site -> Pool backend)
                   -> HandlerFor site (DBRunner site, HandlerFor site ())
#else
defaultGetDBRunner :: YesodPersistBackend site ~ SQL.SqlBackend
                   => (site -> Pool SQL.SqlBackend)
                   -> HandlerFor site (DBRunner site, HandlerFor site ())
#endif
defaultGetDBRunner getPool = do
    pool <- fmap getPool getYesod
    let withPrep conn f = f (persistBackend conn) (SQL.getStmtConn $ persistBackend conn)
    (relKey, (conn, local)) <- allocate
        (do
            (conn, local) <- takeResource pool
#if MIN_VERSION_persistent(2,9,0)
            withPrep conn (\c f -> SQL.connBegin c f Nothing)
#else
            withPrep conn SQL.connBegin
#endif
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
            => ConduitT () a (YesodDB site) ()
            -> ConduitT () a (HandlerFor site) ()
runDBSource src = do
    (dbrunner, cleanup) <- lift getDBRunner
    transPipe (runDBRunner dbrunner) src
    lift cleanup

-- | Extends 'respondSource' to create a streaming database response body.
respondSourceDB :: YesodPersistRunner site
                => ContentType
                -> ConduitT () (Flush Builder) (YesodDB site) ()
                -> HandlerFor site TypedContent
respondSourceDB ctype = respondSource ctype . runDBSource

-- | Get the given entity by ID, or return a 404 not found if it doesn't exist.
#if MIN_VERSION_persistent(2,5,0)
get404 :: (MonadIO m, PersistStoreRead backend, PersistRecordBackend val backend)
       => Key val
       -> ReaderT backend m val
#else
get404 :: (MonadIO m, PersistStore (PersistEntityBackend val), PersistEntity val)
       => Key val
       -> ReaderT (PersistEntityBackend val) m val
#endif
get404 key = do
    mres <- get key
    case mres of
        Nothing -> notFound'
        Just res -> return res

-- | Get the given entity by unique key, or return a 404 not found if it doesn't
--   exist.
#if MIN_VERSION_persistent(2,5,0)
getBy404 :: (PersistUniqueRead backend, PersistRecordBackend val backend, MonadIO m)
         => Unique val
         -> ReaderT backend m (Entity val)
#else
getBy404 :: (PersistUnique (PersistEntityBackend val), PersistEntity val, MonadIO m)
         => Unique val
         -> ReaderT (PersistEntityBackend val) m (Entity val)
#endif
getBy404 key = do
    mres <- getBy key
    case mres of
        Nothing -> notFound'
        Just res -> return res

-- | Create a new record in the database, returning an automatically
-- created key, or raise a 400 bad request if a uniqueness constraint
-- is violated.
--
-- @since 1.4.1
#if MIN_VERSION_persistent(2,14,0)
insert400
    :: (MonadIO m, PersistUniqueWrite backend, PersistRecordBackend val backend, SafeToInsert val)
    => val
    -> ReaderT backend m (Key val)
#elif MIN_VERSION_persistent(2,5,0)
insert400
    :: (MonadIO m, PersistUniqueWrite backend, PersistRecordBackend val backend)
    => val
    -> ReaderT backend m (Key val)
#else
insert400
    :: (MonadIO m, PersistUnique (PersistEntityBackend val), PersistEntity val)
    => val
    -> ReaderT (PersistEntityBackend val) m (Key val)
#endif
insert400 datum = do
    conflict <- checkUnique datum
    case conflict of
        Just unique ->
#if MIN_VERSION_persistent(2, 12, 0)
-- toList is called here because persistent-2.13 changed this
-- to a nonempty list. for versions of persistent prior to 2.13, toList
-- will be a no-op. for persistent-2.13, it'll convert the NonEmptyList to
-- a List.
            badRequest' $ map (unFieldNameHS . fst) $ toList $ persistUniqueToFieldNames unique
#else
            badRequest' $ map (unHaskellName . fst) $ persistUniqueToFieldNames unique
#endif
        Nothing -> insert datum

-- | Same as 'insert400', but doesn’t return a key.
--
-- @since 1.4.1
#if MIN_VERSION_persistent(2,14,0)
insert400_ :: (MonadIO m, PersistUniqueWrite backend, PersistRecordBackend val backend, SafeToInsert val)
           => val
           -> ReaderT backend m ()

#elif MIN_VERSION_persistent(2,5,0)
insert400_ :: (MonadIO m, PersistUniqueWrite backend, PersistRecordBackend val backend)
           => val
           -> ReaderT backend m ()
#else
insert400_ :: (MonadIO m, PersistUnique (PersistEntityBackend val), PersistEntity val)
           => val
           -> ReaderT (PersistEntityBackend val) m ()
#endif
insert400_ datum = insert400 datum >> return ()

-- | Should be equivalent to @lift . notFound@, but there's an apparent bug in
-- GHC 7.4.2 that leads to segfaults. This is a workaround.
notFound' :: MonadIO m => m a
notFound' = liftIO $ throwIO $ HCError NotFound

-- | Constructed like 'notFound'', and for the same reasons.
badRequest' :: MonadIO m => Texts -> m a
badRequest' = liftIO . throwIO . HCError . InvalidArgs
