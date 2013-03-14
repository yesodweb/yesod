{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Yesod.Core.Class.Handler where

import Yesod.Core.Types
import Data.IORef.Lifted (atomicModifyIORef)
import Control.Exception.Lifted (throwIO)
import Control.Monad.Base
import Data.Monoid (mempty)

class Monad m => HandlerReader m where
    type HandlerSite m

    askYesodRequest :: m YesodRequest
    askHandlerEnv :: m (RunHandlerEnv (HandlerSite m))

instance Monad m => HandlerReader (HandlerT site m) where
    type HandlerSite (HandlerT site m) = site

    askYesodRequest = HandlerT $ return . handlerRequest
    askHandlerEnv = HandlerT $ return . handlerEnv

instance Monad m => HandlerReader (WidgetT site m) where
    type HandlerSite (WidgetT site m) = site

    askYesodRequest = WidgetT $ return . (, mempty) . handlerRequest
    askHandlerEnv = WidgetT $ return . (, mempty) . handlerEnv

class HandlerReader m => HandlerState m where
    stateGHState :: (GHState -> (a, GHState)) -> m a

    getGHState :: m GHState
    getGHState = stateGHState $ \s -> (s, s)

    putGHState :: GHState -> m ()
    putGHState s = stateGHState $ const ((), s)

instance MonadBase IO m => HandlerState (HandlerT site m) where
    stateGHState f =
        HandlerT $ flip atomicModifyIORef f' . handlerState
      where
        f' z = let (x, y) = f z in (y, x)

instance MonadBase IO m => HandlerState (WidgetT site m) where
    stateGHState f =
        WidgetT $ fmap (, mempty) . flip atomicModifyIORef f' . handlerState
      where
        f' z = let (x, y) = f z in (y, x)

class HandlerReader m => HandlerError m where
    handlerError :: HandlerContents -> m a

instance MonadBase IO m => HandlerError (HandlerT site m) where
    handlerError = throwIO

instance MonadBase IO m => HandlerError (WidgetT site m) where
    handlerError = throwIO
