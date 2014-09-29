{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Yesod.Core.Class.Handler
    ( MonadHandler (..)
    , MonadWidget (..)
    ) where

import Yesod.Core.Types
import Data.Monoid (mempty)
import Control.Monad (liftM)
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

instance MonadResourceBase m => MonadHandler (WidgetT site m) where
    type HandlerSite (WidgetT site m) = site
    liftHandlerT (HandlerT f) = WidgetT $ liftIO . liftM (, mempty) . f . replaceToParent
{-# RULES "liftHandlerT (WidgetT site IO)" forall f. liftHandlerT (HandlerT f) = WidgetT $ liftM (, mempty) . f #-}

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

class MonadHandler m => MonadWidget m where
    liftWidgetT :: WidgetT (HandlerSite m) IO a -> m a
instance MonadResourceBase m => MonadWidget (WidgetT site m) where
    liftWidgetT (WidgetT f) = WidgetT $ liftIO . f . replaceToParent

#define GO(T) instance MonadWidget m => MonadWidget (T m) where liftWidgetT = lift . liftWidgetT
#define GOX(X, T) instance (X, MonadWidget m) => MonadWidget (T m) where liftWidgetT = lift . liftWidgetT
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
