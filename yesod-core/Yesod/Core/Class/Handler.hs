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
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (MonadResource, MonadResourceBase, ExceptionT (..))
import Control.Monad.Trans.Class (lift)

class MonadResource m => MonadHandler m where
    type HandlerSite m
    liftHandlerT :: HandlerT (HandlerSite m) IO a -> m a

replaceToParent :: HandlerData site route -> HandlerData site ()
replaceToParent hd = hd { handlerToParent = const () }

instance MonadResourceBase m => MonadHandler (HandlerT site m) where
    type HandlerSite (HandlerT site m) = site
    liftHandlerT (HandlerT f) = HandlerT $ liftIO . f . replaceToParent
{-# RULES "liftHandlerT (HandlerT site IO)" forall action. liftHandlerT action = id #-}

instance MonadResourceBase m => MonadHandler (WidgetT site m) where
    type HandlerSite (WidgetT site m) = site
    liftHandlerT (HandlerT f) = WidgetT $ liftIO . liftM (, mempty) . f . replaceToParent
{-# RULES "liftHandlerT (WidgetT site IO)" forall f. liftHandlerT (HandlerT f) = WidgetT $ liftM (, mempty) . f #-}

instance MonadHandler m => MonadHandler (ExceptionT m) where
    type HandlerSite (ExceptionT m) = HandlerSite m
    liftHandlerT = lift . liftHandlerT
-- FIXME add a bunch of transformer instances

class MonadHandler m => MonadWidget m where
    liftWidgetT :: WidgetT (HandlerSite m) IO a -> m a
instance MonadResourceBase m => MonadWidget (WidgetT site m) where
    liftWidgetT (WidgetT f) = WidgetT $ liftIO . f . replaceToParent
-- FIXME add a bunch of transformer instances
