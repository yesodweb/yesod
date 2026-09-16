{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Structural views of the endpoints directly owned by a route fragment.
-- Experimental: generated with @setRouteLeafViews True@.
module Yesod.Routes.Class.Leaf
    ( HasAuthDispatch (..)
    , fillInNested
    , RouteLeaves (..)
    , SomeRouteLeaf (..)
    , Subroute
    , SubrouteDict (..)
    , Dict (..)
    , withRouteLeaf
    ) where

import Data.Constraint (Dict (..))
import Data.Kind (Constraint, Type)
import Yesod.Routes.Class

-- | A fragment's direct endpoints, excluding delegation constructors.
-- Purely delegating fragments do not need this instance.
class RenderRouteNested route => HasAuthDispatch route where
    -- | Direct endpoint constructors and their captures, with delegation
    -- constructors omitted. Generated constructor names start with @Auth@.
    data AuthDispatch route :: Type
    -- | A shallow projection. 'Nothing' means a delegation constructor.
    projectAuthDispatch :: route -> Maybe (AuthDispatch route)
    -- | Embed a local endpoint back into its original fragment.
    fromAuthDispatch :: AuthDispatch route -> route

-- | Adapt a local-endpoint policy to an interface accepting a whole fragment.
-- Only the chosen branch is evaluated. This does not recurse into children.
fillInNested
    :: HasAuthDispatch route
    => (AuthDispatch route -> result)
    -> result
    -> route
    -> result
fillInNested onLeaf onNested = maybe onNested onLeaf . projectAuthDispatch

-- | A generated witness for an endpoint-owning fragment within a route tree.
-- No witness is generated for a fragment that only delegates.
data family Subroute root :: Type -> Type

-- | Generated once, polymorphic in @constraint@, with one constraint in its
-- context per endpoint-owning fragment. Concrete dictionaries are required
-- where this instance is used, not where route data is generated.
class SubrouteDict (constraint :: Type -> Constraint) root where
    getSubrouteDict :: Subroute root route -> Dict (constraint route)

-- | The selected endpoint, including captures consumed by every ancestor.
data SomeRouteLeaf site where
    SomeRouteLeaf
        :: (HasAuthDispatch route, ParentSite route ~ site)
        => Subroute (Route site) route
        -> ParentArgs route
        -> AuthDispatch route
        -> SomeRouteLeaf site

-- | Project a matched route to its deepest endpoint-owning fragment.
-- Subsite mounts are leaves in the parent site; projection stops at the mount.
class RenderRoute site => RouteLeaves site where
    routeLeaf :: Route site -> SomeRouteLeaf site

-- | Visit the selected endpoint with a caller-chosen constraint. Parent
-- authorizers are not invoked, and there is no ancestor fallback.
withRouteLeaf
    :: forall constraint site result.
       (RouteLeaves site, SubrouteDict constraint (Route site))
    => Route site
    -> (forall route.
           (HasAuthDispatch route, ParentSite route ~ site, constraint route)
           => ParentArgs route -> AuthDispatch route -> result)
    -> result
withRouteLeaf matched callback =
    case routeLeaf matched of
        SomeRouteLeaf witness args leaf ->
            case getSubrouteDict @constraint witness of
                Dict -> callback args leaf
