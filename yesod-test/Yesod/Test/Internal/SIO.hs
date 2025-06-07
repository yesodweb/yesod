{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | The 'SIO' type is used by "Yesod.Test" to provide exception-safe
-- environment between requests and assertions.
--
-- This module is internal. Breaking changes to this module will not be
-- reflected in the major version of this package.
--
-- @since 1.6.13
module Yesod.Test.Internal.SIO where

import Control.Monad.Trans.Reader (ReaderT (..))
import Conduit (MonadThrow)
import qualified Control.Monad.State.Class as MS
import Yesod.Core
import Data.IORef
import GHC.Stack (HasCallStack)

-- | State + IO
--
-- @since 1.6.0
newtype SIO s a = SIO (ReaderT (IORef s) IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadUnliftIO)

-- | @since 1.6.23
instance MonadFail (SIO s) where
  fail :: HasCallStack => String -> SIO s a
  fail = error

instance MS.MonadState s (SIO s)
  where
  get = getSIO
  put = putSIO

-- | Retrieve the current state in the 'SIO' type.
--
-- Equivalent to 'MS.get'
--
-- @since 1.6.13
getSIO :: SIO s s
getSIO = SIO $ ReaderT readIORef

-- | Put the given @s@ into the 'SIO' state for later retrieval.
--
-- Equivalent to 'MS.put', but the value is evaluated to weak head normal
-- form.
--
-- @since 1.6.13
putSIO :: s -> SIO s ()
putSIO s = SIO $ ReaderT $ \ref -> writeIORef ref $! s

-- | Modify the underlying @s@ state.
--
-- This is strict in the function used, and is equivalent to 'MS.modify''.
--
-- @since 1.6.13
modifySIO :: (s -> s) -> SIO s ()
modifySIO f = SIO $ ReaderT $ \ref -> modifyIORef' ref f

-- | Run an 'SIO' action with the intial state @s@ provided, returning the
-- result, and discard the final state.
--
-- @since 1.6.13
evalSIO :: SIO s a -> s -> IO a
evalSIO action =
    fmap snd . runSIO action

-- | Run an 'SIO' action with the initial state @s@ provided, returning the
-- final state, and discarding the result.
--
-- @since 1.6.13
execSIO :: SIO s () -> s -> IO s
execSIO action =
    fmap fst . runSIO action

-- | Run an 'SIO' action with the initial state provided, returning both
-- the result of the computation as well as the final state.
--
-- @since 1.6.13
runSIO :: SIO s a -> s -> IO (s, a)
runSIO (SIO (ReaderT f)) s = do
    ref <- newIORef s
    a <- f ref
    s' <- readIORef ref
    pure (s', a)
