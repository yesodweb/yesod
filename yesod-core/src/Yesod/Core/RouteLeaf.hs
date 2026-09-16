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
    , withRouteLeaves
    , withRouteLeavesWithParentArgs
    ) where

import Yesod.Core.Handler (getCurrentRoute)
import Yesod.Core.Types (HandlerFor)
import Yesod.Routes.Class
import Yesod.Routes.Class.Leaf

-- | Fetch the deepest matched endpoint as an existential package. The request
-- selects the fragment; the package retains its local 'RouteLeaves' value,
-- parent captures, and a witness for recovering a caller-chosen constraint.
--
-- No policy dictionaries are needed to fetch it. 'Nothing' means no current
-- route, including subsite misses that supply no parent route. Matched-path
-- 405s still have a route. Applications that bypass the parent runner also
-- bypass its middleware.
getDeepestLeaves
    :: RouteLeafSelection site
    => HandlerFor site (Maybe (SomeRouteLeaf site))
getDeepestLeaves = fmap selectRouteLeaf <$> getCurrentRoute

-- | Visit the deepest matched leaf using a constraint selected with type
-- application, such as @withRouteLeaves \@MyConstraint callback@. The generated
-- dictionary supplies the instance for the fragment selected by the request.
-- 'Nothing' means no current route, never a missing instance.
--
-- The callback is pure in @result@: if it returns a handler action, the caller
-- must execute that returned action. Use 'withRouteLeavesWithParentArgs' for
-- policies that also need ancestor captures.
withRouteLeaves
    :: forall constraint site result.
       (RouteLeafSelection site, RouteFragmentDict constraint (Route site))
    => (forall fragment.
           (HasRouteLeaves fragment, ParentSite fragment ~ site, constraint fragment)
           => RouteLeaves fragment -> result)
    -> HandlerFor site (Maybe result)
withRouteLeaves callback = withRouteLeavesWithParentArgs @constraint $ \_ leaves -> callback leaves

-- | Like 'withRouteLeaves', also passing the selected fragment's parent
-- captures. Selection and dictionary elimination are pure; this wrapper only
-- reads the current route. The callback's result is returned without executing
-- it, even when that result is itself a handler action.
withRouteLeavesWithParentArgs
    :: forall constraint site result.
       (RouteLeafSelection site, RouteFragmentDict constraint (Route site))
    => (forall fragment.
           (HasRouteLeaves fragment, ParentSite fragment ~ site, constraint fragment)
           => ParentArgs fragment -> RouteLeaves fragment -> result)
    -> HandlerFor site (Maybe result)
withRouteLeavesWithParentArgs callback = do
    selected <- getDeepestLeaves
    pure $ fmap (\leaves -> withSomeRouteLeaf @constraint leaves callback) selected
