{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module YesodCoreTest.RouteLeaf.Policy where

import Control.Monad (forM_)
import Yesod.Core
import Yesod.Core.RouteLeaf

class HasRouteLeaves route => AuthorizeRoute route where
    authorizeRoute :: ParentArgs route -> RouteLeaves route -> HandlerFor (ParentSite route) AuthResult

-- Application-owned response policy; this example returns 401 for a missing login.
enforceAuthorization :: AuthResult -> HandlerFor site ()
enforceAuthorization Authorized = pure ()
enforceAuthorization AuthenticationRequired = notAuthenticated
enforceAuthorization (Unauthorized message) = permissionDenied message

-- This module knows no site or concrete authorizer. Construction supplies the
-- entire dictionary; a focused test can instead supply just its leaf policy.
authorizationMiddleware
    :: forall site a.
       (Yesod site, RouteLeafSelection site, RouteFragmentDict AuthorizeRoute (Route site))
    => HandlerFor site a -> HandlerFor site a
authorizationMiddleware handler = defaultYesodMiddleware $ do
    authorization <- withRouteLeavesWithParentArgs @AuthorizeRoute authorizeRoute
    forM_ authorization enforceAuthorization
    handler -- explicit policy: skip authorization when there is no current route
