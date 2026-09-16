{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module YesodCoreTest.RouteLeaf.Policy where

import Yesod.Core hiding (isAuthorized)
import Yesod.Core.RouteLeaf

class HasAuthDispatch route => AuthorizeRoute route where
    isAuthorized :: ParentArgs route -> AuthDispatch route -> HandlerFor (ParentSite route) AuthResult

-- This module knows no site or concrete authorizer. Construction supplies the
-- entire dictionary; a focused test can instead supply just its leaf policy.
authorizationMiddleware
    :: forall site a.
       (Yesod site, RouteLeaves site, SubrouteDict AuthorizeRoute (Route site))
    => HandlerFor site a -> HandlerFor site a
authorizationMiddleware handler = defaultYesodMiddlewareNoAuthCheck $ do
    checked <- getDeepestSubrouteWithInstance @AuthorizeRoute $ \args leaf ->
        dispatchAuthorizationCheck (const $ isAuthorized args leaf)
    case checked of
        Nothing -> handler -- explicit unmatched-route policy
        Just () -> handler
