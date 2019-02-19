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
    , liftHandlerT
    , liftWidgetT
    ) where

import Yesod.Core.Types
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Trans.Class (lift)
import Data.Conduit.Internal (Pipe, ConduitM)

import Control.Monad.Trans.Identity ( IdentityT)
import Control.Monad.Trans.List     ( ListT    )
import Control.Monad.Trans.Maybe    ( MaybeT   )
import Control.Monad.Trans.Except   ( ExceptT  )
import Control.Monad.Trans.Reader   ( ReaderT  )
import Control.Monad.Trans.State    ( StateT   )
import Control.Monad.Trans.Writer   ( WriterT  )
import Control.Monad.Trans.RWS      ( RWST     )
import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT )

-- FIXME should we just use MonadReader instances instead?
class (MonadResource m, MonadLogger m) => MonadHandler m where
    type HandlerSite m
    type SubHandlerSite m
    liftHandler :: HandlerFor (HandlerSite m) a -> m a
    liftSubHandler :: SubHandlerFor (SubHandlerSite m) (HandlerSite m) a -> m a

liftHandlerT :: MonadHandler m => HandlerFor (HandlerSite m) a -> m a
liftHandlerT = liftHandler
{-# DEPRECATED liftHandlerT "Use liftHandler instead" #-}

instance MonadHandler (HandlerFor site) where
    type HandlerSite (HandlerFor site) = site
    type SubHandlerSite (HandlerFor site) = site
    liftHandler = id
    {-# INLINE liftHandler #-}
    liftSubHandler (SubHandlerFor f) = HandlerFor f
    {-# INLINE liftSubHandler #-}

instance MonadHandler (SubHandlerFor sub master) where
    type HandlerSite (SubHandlerFor sub master) = master
    type SubHandlerSite (SubHandlerFor sub master) = sub
    liftHandler (HandlerFor f) = SubHandlerFor $ \hd -> f hd
      { handlerEnv =
          let rhe = handlerEnv hd
           in rhe
                { rheRoute = fmap (rheRouteToMaster rhe) (rheRoute rhe)
                , rheRouteToMaster = id
                , rheChild = rheSite rhe
                }
      }
    {-# INLINE liftHandler #-}
    liftSubHandler = id
    {-# INLINE liftSubHandler #-}

instance MonadHandler (WidgetFor site) where
    type HandlerSite (WidgetFor site) = site
    type SubHandlerSite (WidgetFor site) = site
    liftHandler (HandlerFor f) = WidgetFor $ f . wdHandler
    {-# INLINE liftHandler #-}
    liftSubHandler (SubHandlerFor f) = WidgetFor $ f . wdHandler
    {-# INLINE liftSubHandler #-}

#define GO(T) instance MonadHandler m => MonadHandler (T m) where type HandlerSite (T m) = HandlerSite m; type SubHandlerSite (T m) = SubHandlerSite m; liftHandler = lift . liftHandler; liftSubHandler = lift . liftSubHandler
#define GOX(X, T) instance (X, MonadHandler m) => MonadHandler (T m) where type HandlerSite (T m) = HandlerSite m; type SubHandlerSite (T m) = SubHandlerSite m; liftHandler = lift . liftHandler; liftSubHandler = lift . liftSubHandler
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GO(ExceptT e)
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
    liftWidget :: WidgetFor (HandlerSite m) a -> m a
instance MonadWidget (WidgetFor site) where
    liftWidget = id
    {-# INLINE liftWidget #-}

liftWidgetT :: MonadWidget m => WidgetFor (HandlerSite m) a -> m a
liftWidgetT = liftWidget
{-# DEPRECATED liftWidgetT "Use liftWidget instead" #-}

#define GO(T) instance MonadWidget m => MonadWidget (T m) where liftWidget = lift . liftWidget
#define GOX(X, T) instance (X, MonadWidget m) => MonadWidget (T m) where liftWidget = lift . liftWidget
GO(IdentityT)
GO(ListT)
GO(MaybeT)
GO(ExceptT e)
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
