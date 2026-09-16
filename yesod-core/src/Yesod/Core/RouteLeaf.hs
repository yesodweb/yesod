{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Experimental endpoint reflection for ordinary Yesod middleware.
-- Enable structural generation with @setRouteLeafViews True@ on the shared
-- route options. No dispatch authorization hook is needed.
--
-- The application chooses and interprets the policy constraint. Construct
-- middleware with its concrete dictionaries at application assembly, then
-- supply it to the foundation as a value if the foundation must stay
-- independent of the policy modules.
module Yesod.Core.RouteLeaf
    ( module Yesod.Routes.Class.Leaf
    , getDeepestSubrouteWithInstance
    ) where

import Yesod.Core.Handler (getCurrentRoute)
import Yesod.Core.Types (HandlerFor)
import Yesod.Routes.Class
import Yesod.Routes.Class.Leaf

-- | Visit the current endpoint using a constraint selected with type
-- application. 'Nothing' means there is no current route (for example a 404),
-- not a missing policy instance. Matched-path 405s still have a route.
--
-- Middleware cannot enforce checks on applications that bypass the parent
-- runner, or recover a mount route on subsite misses that supply no route.
getDeepestSubrouteWithInstance
    :: forall constraint site result.
       (RouteLeaves site, SubrouteDict constraint (Route site))
    => (forall route.
           (HasAuthDispatch route, ParentSite route ~ site, constraint route)
           => ParentArgs route -> AuthDispatch route -> HandlerFor site result)
    -> HandlerFor site (Maybe result)
getDeepestSubrouteWithInstance callback = do
    current <- getCurrentRoute
    case current of
        Nothing -> pure Nothing
        Just matched -> Just <$> withRouteLeaf @constraint matched callback
