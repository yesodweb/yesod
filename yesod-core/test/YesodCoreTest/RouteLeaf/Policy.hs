{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module YesodCoreTest.RouteLeaf.Policy where

import Yesod.Core hiding (isAuthorized)
import Yesod.Core.RouteLeaf

class HasRouteLeaf route => AuthorizeRoute route where
    isAuthorized :: ParentArgs route -> RouteLeaf route -> HandlerFor (ParentSite route) AuthResult

-- Application-owned response policy; this example returns 401 for a missing login.
enforceAuthorization :: AuthResult -> HandlerFor site ()
enforceAuthorization Authorized = pure ()
enforceAuthorization AuthenticationRequired = notAuthenticated
enforceAuthorization (Unauthorized message) = permissionDenied message

-- This module knows no site or concrete authorizer. Construction supplies the
-- entire dictionary; a focused test can instead supply just its leaf policy.
authorizationMiddleware
    :: forall site a.
       (Yesod site, RouteLeaves site, RouteFragmentDict AuthorizeRoute (Route site))
    => HandlerFor site a -> HandlerFor site a
authorizationMiddleware handler = defaultYesodMiddleware $ do
    checked <- getDeepestSubrouteWithInstance @AuthorizeRoute $ \args leaf ->
        enforceAuthorization =<< isAuthorized args leaf
    case checked of
        Nothing -> handler -- explicit unmatched-route policy
        Just () -> handler
