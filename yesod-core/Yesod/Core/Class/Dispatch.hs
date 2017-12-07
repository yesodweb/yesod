{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts  #-}
module Yesod.Core.Class.Dispatch where

import Yesod.Routes.Class
import qualified Network.Wai as W
import Yesod.Core.Types
import Yesod.Core.Content
import Yesod.Core.Handler (sendWaiApplication, stripHandlerT)
import Yesod.Core.Class.Yesod
import Yesod.Core.Class.Handler

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

instance YesodSubDispatch WaiSubsiteWithAuth (HandlerT master IO) where
  yesodSubDispatch YesodSubRunnerEnv {..} req =
      ysreParentRunner base ysreParentEnv (fmap ysreToParentRoute route) req
    where
      base  = stripHandlerT handlert ysreGetSub ysreToParentRoute route
      route = Just $ WaiSubsiteWithAuthRoute (W.pathInfo req) []
      WaiSubsiteWithAuth set = ysreGetSub $ yreSite $ ysreParentEnv
      handlert = sendWaiApplication $ set

-- | A helper function for creating YesodSubDispatch instances, used by the
-- internal generated code. This function has been exported since 1.4.11.
-- It promotes a subsite handler to a wai application.
subHelper :: Monad m -- NOTE: This is incredibly similar in type signature to yesodRunner, should probably be pointed out/explained.
          => HandlerT child (HandlerT parent m) TypedContent
          -> YesodSubRunnerEnv child parent (HandlerT parent m)
          -> Maybe (Route child)
          -> W.Application
subHelper handlert YesodSubRunnerEnv {..} route =
    ysreParentRunner base ysreParentEnv (fmap ysreToParentRoute route)
  where
    base = stripHandlerT (fmap toTypedContent handlert) ysreGetSub ysreToParentRoute route
