{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The 'SIO' type is used by "Yesod.Test" to provide exception-safe
-- environment between requests and assertions.
--
-- This module is internal. Breaking changes to this module will not be
-- reflected in the major version of this package.
--
-- @since TODO
module Yesod.Test.Internal.SIO where

import Control.Monad.Trans.Reader (ReaderT (..))
import Conduit (MonadThrow)
import qualified Control.Monad.State.Class as MS
import Yesod.Core
import Data.IORef

-- | State + IO
--
-- @since 1.6.0
newtype SIO s a = SIO (ReaderT (IORef s) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadUnliftIO)

instance MS.MonadState s (SIO s)
  where
  get = getSIO
  put = putSIO

getSIO :: SIO s s
getSIO = SIO $ ReaderT readIORef

putSIO :: s -> SIO s ()
putSIO s = SIO $ ReaderT $ \ref -> writeIORef ref $! s

modifySIO :: (s -> s) -> SIO s ()
modifySIO f = SIO $ ReaderT $ \ref -> modifyIORef' ref f

evalSIO :: SIO s a -> s -> IO a
evalSIO (SIO (ReaderT f)) s = newIORef s >>= f

execSIO :: SIO s () -> s -> IO s
execSIO (SIO (ReaderT f)) s = do
  ref <- newIORef s
  f ref
  readIORef ref
