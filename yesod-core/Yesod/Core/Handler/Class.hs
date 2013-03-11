{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Yesod.Core.Handler.Class where

import Yesod.Core.Types
import Yesod.Core.Trans.Class (lift)
import Control.Monad.Trans.Class (MonadTrans)
import Data.IORef.Lifted (atomicModifyIORef)
import Control.Exception.Lifted (throwIO)

class Monad m => HandlerReader m where
    type HandlerReaderSub m
    type HandlerReaderMaster m

    askYesodRequest :: m YesodRequest
    askHandlerEnv :: m (RunHandlerEnv (HandlerReaderSub m) (HandlerReaderMaster m))

instance HandlerReader (GHandler sub master) where
    type HandlerReaderSub (GHandler sub master) = sub
    type HandlerReaderMaster (GHandler sub master) = master

    askYesodRequest = GHandler $ return . handlerRequest
    askHandlerEnv = GHandler $ return . handlerEnv

instance HandlerReader (GWidget sub master) where
    type HandlerReaderSub (GWidget sub master) = sub
    type HandlerReaderMaster (GWidget sub master) = master

    askYesodRequest = lift askYesodRequest
    askHandlerEnv = lift askHandlerEnv

instance (MonadTrans t, HandlerReader m, Monad (t m)) => HandlerReader (t m) where
    type HandlerReaderSub (t m) = HandlerReaderSub m
    type HandlerReaderMaster (t m) = HandlerReaderMaster m

    askYesodRequest = lift askYesodRequest
    askHandlerEnv = lift askHandlerEnv

class HandlerReader m => HandlerState m where
    type HandlerStateSub m
    type HandlerStateMaster m

    stateGHState :: (GHState -> (a, GHState)) -> m a

    getGHState :: m GHState
    getGHState = stateGHState $ \s -> (s, s)

    putGHState :: GHState -> m ()
    putGHState s = stateGHState $ const ((), s)

instance HandlerState (GHandler sub master) where
    type HandlerStateSub (GHandler sub master) = sub
    type HandlerStateMaster (GHandler sub master) = master

    stateGHState f =
        GHandler $ flip atomicModifyIORef f' . handlerState
      where
        f' z = let (x, y) = f z in (y, x)

instance HandlerState (GWidget sub master) where
    type HandlerStateSub (GWidget sub master) = sub
    type HandlerStateMaster (GWidget sub master) = master

    stateGHState = lift . stateGHState

instance (MonadTrans t, HandlerState m, Monad (t m)) => HandlerState (t m) where
    type HandlerStateSub (t m) = HandlerStateSub m
    type HandlerStateMaster (t m) = HandlerStateMaster m

    stateGHState = lift . stateGHState

class Monad m => HandlerError m where
    handlerError :: ErrorResponse -> m a

instance HandlerError (GHandler sub master) where
    handlerError = throwIO . HCError

instance HandlerError (GWidget sub master) where
    handlerError = lift . handlerError

instance (HandlerError m, MonadTrans t, Monad (t m)) => HandlerError (t m) where
    handlerError = lift . handlerError
