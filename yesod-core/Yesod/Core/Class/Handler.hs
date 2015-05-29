{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Yesod.Core.Class.Handler
    ( MonadHandler (..)
    ) where

import Yesod.Core.Types
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource, MonadResourceBase)
import Control.Monad.Trans.Class (lift)
import Data.Monoid (Monoid)
import Data.Conduit.Internal (Pipe, ConduitM)

import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Error    ( ErrorT, Error)
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )
import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

class MonadResource m => MonadHandler m where
    type HandlerSite m
    liftHandlerT :: HandlerT (HandlerSite m) IO a -> m a

replaceToParent :: HandlerData site route -> HandlerData site ()
replaceToParent hd = hd { handlerToParent = const () }

instance MonadResourceBase m => MonadHandler (HandlerT site m) where
    type HandlerSite (HandlerT site m) = site
    liftHandlerT (HandlerT f) = HandlerT $ liftIO . f . replaceToParent
{-# RULES "liftHandlerT (HandlerT site IO)" liftHandlerT = id #-}

#define GO(T) instance MonadHandler m => MonadHandler (T m) where type HandlerSite (T m) = HandlerSite m; liftHandlerT = lift . liftHandlerT
#define GOX(X, T) instance (X, MonadHandler m) => MonadHandler (T m) where type HandlerSite (T m) = HandlerSite m; liftHandlerT = lift . liftHandlerT
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GOX(Error e, ErrorT e)
GO(ReaderT r)
GO(StateT s)
GOX(Monoid w, WriterT w)
GOX(Monoid w, RWST r w s)
GOX(Monoid w, Strict.RWST r w s)
GO(Strict.StateT s)
GOX(Monoid w, Strict.WriterT w)
GO(Pipe l i o u)
GO(ConduitM i o)
#undef GO
#undef GOX
