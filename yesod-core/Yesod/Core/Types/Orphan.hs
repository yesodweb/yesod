{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances #-}
module Yesod.Core.Types.Orphan where

import Yesod.Core.Types
import Control.Monad.Trans.Class
import           Control.Monad.IO.Class             (MonadIO (liftIO))
import           Control.Monad.Base                 (MonadBase (liftBase))
import           Control.Monad.Trans.Control        (MonadBaseControl (..))
import           Control.Monad.Trans.Resource       (MonadResource (..))
import           Data.Conduit                       (MonadThrow (..))

instance MonadTrans (HandlerT sub) where
    lift = HandlerT . const
instance MonadBase b m => MonadBase b (HandlerT sub m) where
    liftBase = lift . liftBase
instance MonadBaseControl b m => MonadBaseControl b (HandlerT sub m)
instance MonadResource m => MonadResource (HandlerT sub m) where
    liftResourceT = lift . liftResourceT
instance MonadIO m => MonadIO (HandlerT sub m)
instance MonadThrow m => MonadThrow (HandlerT sub m) where
    monadThrow = lift . monadThrow
