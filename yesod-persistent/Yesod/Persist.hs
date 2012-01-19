{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Persist
    ( YesodPersist (..)
    , YesodDB
    , get404
    , getBy404
    , module Database.Persist
    , module Database.Persist.TH
    ) where

import Database.Persist
import Database.Persist.TH
import Control.Monad.Trans.Class (MonadTrans)

import Yesod.Handler

type YesodDB sub master = YesodPersistBackend master (GHandler sub master)

class YesodPersist master where
    type YesodPersistBackend master :: (* -> *) -> * -> *
    runDB :: YesodDB sub master a -> GHandler sub master a

-- | Get the given entity by ID, or return a 404 not found if it doesn't exist.
get404 :: ( PersistStore b m
          , PersistEntity val
          , Monad (b m)
          , m ~ GHandler sub master
          , MonadTrans b
          )
       => Key b val -> b m val
get404 key = do
    mres <- get key
    case mres of
        Nothing -> lift notFound
        Just res -> return res

-- | Get the given entity by unique key, or return a 404 not found if it doesn't
--   exist.
getBy404 :: ( PersistUnique b m
            , PersistEntity val
            , m ~ GHandler sub master
            , Monad (b m)
            , MonadTrans b
            )
         => Unique val b -> b m (Entity b val)
getBy404 key = do
    mres <- getBy key
    case mres of
        Nothing -> lift notFound
        Just res -> return res
