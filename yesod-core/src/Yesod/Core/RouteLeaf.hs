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
    , getDeepestLeaves
    , getDeepestLeavesWithParentArgs
    , getDeepestSubrouteWithInstance
    ) where

import Yesod.Core.Handler (getCurrentRoute)
import Yesod.Core.Types (HandlerFor)
import Yesod.Routes.Class
import Yesod.Routes.Class.Leaf

-- | Fetch the current endpoint only if it is directly owned by @fragment@.
-- For example, @getDeepestLeaves \@AccountR@ excludes endpoints owned by children
-- of @AccountR@. Select the fragment with type application or the result type.
--
-- 'Nothing' means either no current route or an endpoint in another fragment.
-- Use 'getDeepestSubrouteWithInstance' to dispatch across all fragments instead
-- of checking one known fragment. No policy dictionaries are needed here.
getDeepestLeaves
    :: forall fragment.
       (LookupRouteLeaves fragment, RouteLeafSelection (ParentSite fragment))
    => HandlerFor (ParentSite fragment) (Maybe (RouteLeaves fragment))
getDeepestLeaves = fmap (fmap snd) (getDeepestLeavesWithParentArgs @fragment)

-- | Like 'getDeepestLeaves', retaining ancestor captures for policies that need
-- them. A local leaf contains only its own captures, so parent captures remain
-- separate. Matched-path 405s still have a route; unmatched requests do not.
getDeepestLeavesWithParentArgs
    :: forall fragment.
       (LookupRouteLeaves fragment, RouteLeafSelection (ParentSite fragment))
    => HandlerFor (ParentSite fragment) (Maybe (ParentArgs fragment, RouteLeaves fragment))
getDeepestLeavesWithParentArgs = do
    current <- getCurrentRoute
    pure $ current >>= lookupRouteLeaves @fragment . selectRouteLeaf

-- | Visit the current endpoint using a constraint selected with type
-- application. 'Nothing' means there is no current route (for example a 404),
-- not a missing policy instance. Matched-path 405s still have a route.
--
-- Middleware cannot enforce checks on applications that bypass the parent
-- runner, or recover a mount route on subsite misses that supply no route.
getDeepestSubrouteWithInstance
    :: forall constraint site result.
       (RouteLeafSelection site, RouteFragmentDict constraint (Route site))
    => (forall fragment.
           (HasRouteLeaves fragment, ParentSite fragment ~ site, constraint fragment)
           => ParentArgs fragment -> RouteLeaves fragment -> HandlerFor site result)
    -> HandlerFor site (Maybe result)
getDeepestSubrouteWithInstance callback = do
    current <- getCurrentRoute
    case current of
        Nothing -> pure Nothing
        Just matched -> Just <$> withRouteLeaf @constraint matched callback
