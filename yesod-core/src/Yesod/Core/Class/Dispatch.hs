{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Core.Class.Dispatch where

import qualified Network.Wai as W
import Yesod.Core.Types
import Yesod.Core.Content (ToTypedContent (..))
import Yesod.Core.Handler (sendWaiApplication)
import Yesod.Core.Class.Yesod

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function. You should never need to deal with it directly.
class Yesod site => YesodDispatch site where
    yesodDispatch :: YesodRunnerEnv site -> W.Application

class YesodSubDispatch sub master where
    yesodSubDispatch :: YesodSubRunnerEnv sub master -> W.Application

instance YesodSubDispatch WaiSubsite master where
    yesodSubDispatch YesodSubRunnerEnv {..} = app
      where
        WaiSubsite app = ysreGetSub $ yreSite ysreParentEnv

instance YesodSubDispatch WaiSubsiteWithAuth master where
  yesodSubDispatch YesodSubRunnerEnv {..} req =
      ysreParentRunner handlert ysreParentEnv (fmap ysreToParentRoute route) req
    where
      route = Just $ WaiSubsiteWithAuthRoute (W.pathInfo req) []
      WaiSubsiteWithAuth set = ysreGetSub $ yreSite $ ysreParentEnv
      handlert = sendWaiApplication set

subHelper
  :: ToTypedContent content
  => SubHandlerFor child master content
  -> YesodSubRunnerEnv child master
  -> Maybe (Route child)
  -> W.Application
subHelper (SubHandlerFor f) YesodSubRunnerEnv {..} mroute =
    ysreParentRunner handler ysreParentEnv (fmap ysreToParentRoute mroute)
  where
    handler = fmap toTypedContent $ HandlerFor $ \hd ->
      let rhe = handlerEnv hd
          rhe' = rhe
            { rheRoute = mroute
            , rheChild = ysreGetSub $ yreSite ysreParentEnv
            , rheRouteToMaster = ysreToParentRoute
            }
       in f hd { handlerEnv = rhe' }
