{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Persist
    ( YesodPersist (..)
    , get404
    , getBy404
    , module Database.Persist
    , module Database.Persist.TH
    ) where

import Database.Persist
import Database.Persist.TH
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Failure (Failure)

import Yesod.Handler

class YesodPersist y where
    type YesodDB y :: (* -> *) -> * -> *
    runDB :: YesodDB y (GGHandler sub y IO) a -> GHandler sub y a

-- | Get the given entity by ID, or return a 404 not found if it doesn't exist.
get404 :: (PersistBackend (t m), PersistEntity val, Monad (t m),
           Failure ErrorResponse m, MonadTrans t)
       => Key val -> t m val
get404 key = do
    mres <- get key
    case mres of
        Nothing -> lift notFound
        Just res -> return res

-- | Get the given entity by unique key, or return a 404 not found if it doesn't
--   exist.
getBy404 :: (PersistBackend (t m), PersistEntity val, Monad (t m),
             Failure ErrorResponse m, MonadTrans t)
         => Unique val -> t m (Key val, val)
getBy404 key = do
    mres <- getBy key
    case mres of
        Nothing -> lift notFound
        Just res -> return res
