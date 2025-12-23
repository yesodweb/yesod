{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Core.Class.Dispatch where

import Data.Text (Text)
import qualified Network.Wai as W
import Yesod.Core.Types
import Yesod.Core.Content (ToTypedContent (..))
import Yesod.Core.Handler (sendWaiApplication)
import Yesod.Core.Class.Yesod
import Network.HTTP.Types.Method (Method)
import Yesod.Routes.Class

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function. You should never need to deal with it directly.
class Yesod site => YesodDispatch site where
    yesodDispatch :: YesodRunnerEnv site -> W.Application

-- | This class enables you to dispatch on a route fragment without needing
-- to know how to dispatch on the entire route structure. This allows you
-- to break up route generation into multiple files.
--
-- For details on use, see 'setFocusOnNestedRoute'.
--
-- @since 1.6.28.0
class RenderRouteNested a => YesodDispatchNested a where
    -- | Returns a @'HandlerFor' site 'TypedContent'@ corresponding to the
    -- route fragment provided.
    --
    -- @since 1.6.28.0
    yesodDispatchNested
        :: YesodRunnerEnv (ParentSite a)
        -> ParentArgs a
        -- ^ The parts of the parent route
        -> Method
        -- ^ The HTTP Method invoked from the request.
        -> [Text]
        -- ^ The path fragments, after parsing out the parent.
        -> Maybe (W.Application, Maybe a)
        -- ^ The handler for the route (possibly notFound or badMethod)
        -- along with the parsed route constructor. This returns 'Nothing'
        -- if we are allowing pass-through in nested routes, or @'Just'
        -- 'notFound'@ if we are not.

class YesodDispatch' route site where
    yesodDispatch' :: proxy route -> YesodRunnerEnv site -> W.Application

instance YesodDispatch site => YesodDispatch' (Route site) site where
    yesodDispatch' _ = yesodDispatch

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
