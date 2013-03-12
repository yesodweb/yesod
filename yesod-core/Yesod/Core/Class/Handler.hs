{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Yesod.Core.Class.Handler where

import Yesod.Core.Types
import Yesod.Core.Class.MonadLift (lift)
import Control.Monad.Trans.Class (MonadTrans)
import Data.IORef.Lifted (atomicModifyIORef)
import Control.Exception.Lifted (throwIO)

class Monad m => HandlerReader m where
    type HandlerSub m
    type HandlerMaster m

    askYesodRequest :: m YesodRequest
    askHandlerEnv :: m (RunHandlerEnv (HandlerSub m) (HandlerMaster m))

instance HandlerReader (GHandler sub master) where
    type HandlerSub (GHandler sub master) = sub
    type HandlerMaster (GHandler sub master) = master

    askYesodRequest = GHandler $ return . handlerRequest
    askHandlerEnv = GHandler $ return . handlerEnv

instance HandlerReader (GWidget sub master) where
    type HandlerSub (GWidget sub master) = sub
    type HandlerMaster (GWidget sub master) = master

    askYesodRequest = lift askYesodRequest
    askHandlerEnv = lift askHandlerEnv

instance (MonadTrans t, HandlerReader m, Monad (t m)) => HandlerReader (t m) where
    type HandlerSub (t m) = HandlerSub m
    type HandlerMaster (t m) = HandlerMaster m

    askYesodRequest = lift askYesodRequest
    askHandlerEnv = lift askHandlerEnv

class HandlerReader m => HandlerState m where
    stateGHState :: (GHState -> (a, GHState)) -> m a

    getGHState :: m GHState
    getGHState = stateGHState $ \s -> (s, s)

    putGHState :: GHState -> m ()
    putGHState s = stateGHState $ const ((), s)

instance HandlerState (GHandler sub master) where
    stateGHState f =
        GHandler $ flip atomicModifyIORef f' . handlerState
      where
        f' z = let (x, y) = f z in (y, x)

instance HandlerState (GWidget sub master) where
    stateGHState = lift . stateGHState

instance (MonadTrans t, HandlerState m, Monad (t m)) => HandlerState (t m) where
    stateGHState = lift . stateGHState

class HandlerReader m => HandlerError m where
    handlerError :: HandlerContents -> m a

instance HandlerError (GHandler sub master) where
    handlerError = throwIO

instance HandlerError (GWidget sub master) where
    handlerError = lift . handlerError

instance (HandlerError m, MonadTrans t, Monad (t m)) => HandlerError (t m) where
    handlerError = lift . handlerError
