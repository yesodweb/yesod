{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Yesod.Core.Class.Handler where

import Yesod.Core.Types
import Yesod.Core.Types.Orphan ()
import Yesod.Core.Class.MonadLift (lift)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Resource
import Control.Monad.Trans.Control
import Data.IORef.Lifted (atomicModifyIORef)
import Control.Exception.Lifted (throwIO)

class Monad m => HandlerReader m where
    type HandlerSite m
    type HandlerMaster m

    askYesodRequest :: m YesodRequest
    askHandlerEnv :: m (RunHandlerEnv (HandlerSite m))

instance HandlerReader (GHandler site) where
    type HandlerSite (GHandler site) = site
    type HandlerMaster (GHandler site) = site

    askYesodRequest = GHandler $ return . handlerRequest
    askHandlerEnv = GHandler $ return . handlerEnv

instance HandlerReader m => HandlerReader (HandlerT site m) where
    type HandlerSite (HandlerT site m) = site
    type HandlerMaster (HandlerT site m) = HandlerMaster m

    askYesodRequest = HandlerT $ return . handlerRequest
    askHandlerEnv = HandlerT $ return . handlerEnv

instance HandlerReader (GWidget site) where
    type HandlerSite (GWidget site) = site
    type HandlerMaster (GWidget site) = site

    askYesodRequest = lift askYesodRequest
    askHandlerEnv = lift askHandlerEnv

class HandlerReader m => HandlerState m where
    stateGHState :: (GHState -> (a, GHState)) -> m a

    getGHState :: m GHState
    getGHState = stateGHState $ \s -> (s, s)

    putGHState :: GHState -> m ()
    putGHState s = stateGHState $ const ((), s)

instance HandlerState (GHandler site) where
    stateGHState f =
        GHandler $ flip atomicModifyIORef f' . handlerState
      where
        f' z = let (x, y) = f z in (y, x)

instance HandlerState (GWidget site) where
    stateGHState = lift . stateGHState

instance HandlerState m => HandlerState (HandlerT site m) where
    stateGHState = lift . stateGHState

class HandlerReader m => HandlerError m where
    handlerError :: HandlerContents -> m a

instance HandlerError (GHandler site) where
    handlerError = throwIO

instance HandlerError (GWidget site) where
    handlerError = lift . handlerError

instance HandlerError m => HandlerError (HandlerT site m) where
    handlerError = lift . handlerError
