{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yesod.Core.Class.Dispatch where

import Data.Proxy (Proxy(..))
import qualified Network.Wai as W
import Yesod.Core.Types
import Yesod.Core.Content (ToTypedContent (..))
import Yesod.Core.Handler (sendWaiApplication)
import Yesod.Core.Class.Yesod
import Yesod.Routes.Class

-- | This class is automatically instantiated when you use the template haskell
-- mkYesod function. You should never need to deal with it directly.
class Yesod site => YesodDispatch site where
    yesodDispatch :: YesodRunnerEnv site -> W.Application

class (RenderRoute (ParentSite a)) => ToParentRoute a where
    toParentRoute :: ParentArgs a -> a -> Route (ParentSite a)

instance (RenderRoute a) => ToParentRoute (Route a) where
    toParentRoute _ = id

-- | This class enables you to dispatch on a route fragment without needing
-- to know how to dispatch on the entire route structure. This allows you
-- to break up route generation into multiple files.
--
-- For details on use, see 'setFocusOnNestedRoute'.
--
-- @since 1.6.28.0
class RenderRouteNested a => YesodDispatchNested a where
    -- | Dispatches a request to a nested route fragment.
    --
    -- The implementation uses the full WAI 'Request' to determine if the
    -- remaining path (after the parent route) matches any child routes.
    -- Returns 'Nothing' if no child routes match (for fallthrough to other
    -- routes), or 'Just' a continuation that handles the request.
    --
    -- The parent depth (number of path pieces consumed by the parent route)
    -- is statically known during Template Haskell generation and baked into
    -- the generated instance.
    --
    -- @since 1.6.28.0
    yesodDispatchNested
        :: (Yesod (ParentSite a), ToParentRoute a)
        => Proxy a
        -- ^ Type proxy to resolve ambiguity from non-injective type families
        -> ParentArgs a
        -- ^ The dynamic arguments from the parent route
        -> YesodRunnerEnv (ParentSite a)
        -- ^ The runner environment
        -> W.Request
        -- ^ The full WAI request
        -> Maybe ((W.Response -> IO W.ResponseReceived) -> IO W.ResponseReceived)
        -- ^ Returns 'Nothing' for fallthrough, or 'Just' a continuation
        -- that completes the 'Application' type when given a respond callback

instance YesodDispatch site => YesodDispatchNested (Route site) where
    yesodDispatchNested _ _ yre req = Just $ yesodDispatch yre req

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
