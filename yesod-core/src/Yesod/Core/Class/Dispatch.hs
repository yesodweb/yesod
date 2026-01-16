{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as S
import Data.Proxy (Proxy(..))
import qualified Network.Wai as W
import Yesod.Core.Types
import Yesod.Core.Content (ToTypedContent (..))
import Yesod.Core.Handler (sendWaiApplication, RedirectUrl, notFound)
import           Data.ByteString.Builder      (toLazyByteString, byteString)
import Yesod.Core.Class.Yesod
import Yesod.Routes.Class
import Data.Text (Text)
import Yesod.Core.Internal.Run
import Network.HTTP.Types

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
        :: (Yesod (ParentSite a))
        => Proxy a
        -- ^ Type proxy to resolve ambiguity from non-injective type families
        -> ParentArgs a
        -- ^ The dynamic arguments from the parent route
        -> (a -> Route (ParentSite a))
        -- ^ Function to wrap the nested route in the parent constructor
        -> YesodRunnerEnv (ParentSite a)
        -- ^ The runner environment
        -> W.Request
        -- ^ The full WAI request
        -> Maybe ((W.Response -> IO W.ResponseReceived) -> IO W.ResponseReceived)
        -- ^ Returns 'Nothing' for fallthrough, or 'Just' a continuation
        -- that completes the 'Application' type when given a respond callback

instance YesodDispatch site => YesodDispatchNested (Route site) where
    yesodDispatchNested _ _ _ yre req = Just $ yesodDispatch yre req

class RedirectUrl site url => UrlToDispatch url site where
    urlToDispatch :: url -> YesodRunnerEnv site -> W.Application

instance YesodDispatch site => UrlToDispatch (Route site) site where
    urlToDispatch _ = yesodDispatch

instance (YesodDispatch site, RedirectUrl site (Route site, a)) => UrlToDispatch (Route site, a) site where
    urlToDispatch _ = yesodDispatch

instance YesodDispatch site => UrlToDispatch Text site where
    urlToDispatch _ = yesodDispatch

instance YesodDispatch site => UrlToDispatch String site where
    urlToDispatch _ = yesodDispatch

instance
    ( ParentSite route ~ site
    , YesodDispatchNested route
    , RedirectUrl site (WithParentArgs route)
    , Yesod site
    , ToParentRoute route
    )
  =>
    UrlToDispatch (WithParentArgs route) site
  where
    urlToDispatch (WithParentArgs args _) = toWaiAppYre' (Proxy :: Proxy route) args

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

toWaiAppYre'
    :: (Yesod (ParentSite a), YesodDispatchNested a, ToParentRoute a)
    => Proxy a
    -> ParentArgs a
    -> YesodRunnerEnv (ParentSite a)
    -> W.Application
toWaiAppYre' proxy parentArgs yre req =
    case cleanPath site $ W.pathInfo req of
        Left pieces -> sendRedirect site pieces req
        Right pieces -> do
            let mapplication =
                    yesodDispatchNested proxy parentArgs (toParentRoute parentArgs) yre req
                        { W.pathInfo = pieces
                        }
            case mapplication of
                Nothing ->
                    yesodRunner (notFound :: HandlerFor site ()) yre Nothing req
                Just k ->
                    k
  where
    site = yreSite yre
    sendRedirect :: Yesod master => master -> [Text] -> W.Application
    sendRedirect y segments' env sendResponse =
         sendResponse $ W.responseLBS status
                [ ("Content-Type", "text/plain")
                , ("Location", BL.toStrict $ toLazyByteString dest')
                ] "Redirecting"
      where
        -- Ensure that non-GET requests get redirected correctly. See:
        -- https://github.com/yesodweb/yesod/issues/951
        status
            | W.requestMethod env == "GET" = status301
            | otherwise                    = status307

        dest = joinPath y (resolveApproot y env) segments' []
        dest' =
            if S.null (W.rawQueryString env)
                then dest
                else dest `mappend`
                     byteString (W.rawQueryString env)
