{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts  #-}
module Yesod.Core.Class.Dispatch where

import qualified Network.Wai as W
import Yesod.Core.Types
import Yesod.Core.Content (ToTypedContent (..))
import Yesod.Core.Handler (sendWaiApplication, getYesod, getCurrentRoute)
import Yesod.Core.Class.Handler
import Yesod.Core.Class.Yesod
import Control.Monad.Trans.Reader (ReaderT (..), ask)

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function. You should never need to deal with it directly.
class Yesod site => YesodDispatch site where
    yesodDispatch :: YesodRunnerEnv site -> W.Application

class YesodSubDispatch sub m where
    yesodSubDispatch :: YesodSubRunnerEnv sub (HandlerSite m) m
                     -> W.Application

instance YesodSubDispatch WaiSubsite master where
    yesodSubDispatch YesodSubRunnerEnv {..} = app
      where
        WaiSubsite app = ysreGetSub $ yreSite ysreParentEnv

instance MonadHandler m => YesodSubDispatch WaiSubsiteWithAuth m where
  yesodSubDispatch YesodSubRunnerEnv {..} req =
      ysreParentRunner handlert ysreParentEnv (fmap ysreToParentRoute route) req
    where
      route = Just $ WaiSubsiteWithAuthRoute (W.pathInfo req) []
      WaiSubsiteWithAuth set = ysreGetSub $ yreSite $ ysreParentEnv
      handlert = sendWaiApplication set

data SubsiteData child parent = SubsiteData
  { sdRouteToParent :: !(Route child -> Route parent)
  , sdCurrentRoute :: !(Maybe (Route child))
  , sdSubsiteData :: !child
  }

class MonadHandler m => MonadSubHandler m where
  type SubHandlerSite m

  liftSubHandler :: ReaderT (SubsiteData (SubHandlerSite m) (HandlerSite m)) (HandlerFor (HandlerSite m)) a -> m a

getSubYesod :: MonadSubHandler m => m (SubHandlerSite m)
getSubYesod = liftSubHandler $ ReaderT $ return . sdSubsiteData

getRouteToParent :: MonadSubHandler m => m (Route (SubHandlerSite m) -> Route (HandlerSite m))
getRouteToParent = liftSubHandler $ ReaderT $ return . sdRouteToParent

getSubCurrentRoute :: MonadSubHandler m => m (Maybe (Route (SubHandlerSite m)))
getSubCurrentRoute = liftSubHandler $ ReaderT $ return . sdCurrentRoute

instance MonadSubHandler (HandlerFor site) where
  type SubHandlerSite (HandlerFor site) = site

  liftSubHandler (ReaderT x) = do
    parent <- getYesod
    currentRoute <- getCurrentRoute
    x SubsiteData
      { sdRouteToParent = id
      , sdCurrentRoute = currentRoute
      , sdSubsiteData = parent
      }

instance MonadSubHandler (WidgetFor site) where
  type SubHandlerSite (WidgetFor site) = site

  liftSubHandler (ReaderT x) = do
    parent <- getYesod
    currentRoute <- getCurrentRoute
    liftHandler $ x SubsiteData
      { sdRouteToParent = id
      , sdCurrentRoute = currentRoute
      , sdSubsiteData = parent
      }

instance (MonadSubHandler m, parent ~ SubHandlerSite m) => MonadSubHandler (ReaderT (SubsiteData child parent) m) where
  type SubHandlerSite (ReaderT (SubsiteData child parent) m) = child

  liftSubHandler (ReaderT f) = ReaderT $ \env -> do
    toParent' <- getRouteToParent
    liftHandler $ f env
      { sdRouteToParent = toParent' . sdRouteToParent env
      }

subHelper
  :: (ToTypedContent content, MonadSubHandler m, master ~ HandlerSite m, parent ~ SubHandlerSite m)
  => ReaderT (SubsiteData child master) m content
  -> YesodSubRunnerEnv child parent m
  -> Maybe (Route child)
  -> W.Application
subHelper (ReaderT f) YesodSubRunnerEnv {..} mroute =
    ysreParentRunner handler ysreParentEnv (fmap ysreToParentRoute mroute)
  where
    handler = fmap toTypedContent $ do
      tm <- getRouteToParent
      f SubsiteData
        { sdRouteToParent = tm . ysreToParentRoute
        , sdCurrentRoute = mroute
        , sdSubsiteData = ysreGetSub $ yreSite ysreParentEnv
        }
