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
    ( HasRouteLeaf (..)
    , fillInNested
    , RouteLeaves (..)
    , SomeRouteLeaf (..)
    , RouteFragmentWitness
    , RouteFragmentDict (..)
    , Dict (..)
    , withRouteLeaf
    ) where

import Data.Constraint (Dict (..))
import Data.Kind (Constraint, Type)
import Yesod.Routes.Class

-- | A fragment's direct endpoints, excluding delegation constructors.
-- Purely delegating fragments do not need this instance.
class RenderRouteNested fragment => HasRouteLeaf fragment where
    -- | Direct endpoint constructors and their captures, with delegation
    -- constructors omitted. Generated constructor names start with @Leaf@.
    data RouteLeaf fragment :: Type
    -- | A shallow projection. 'Nothing' means a delegation constructor.
    projectRouteLeaf :: fragment -> Maybe (RouteLeaf fragment)
    -- | Embed a local endpoint back into its original fragment.
    fromRouteLeaf :: RouteLeaf fragment -> fragment

-- | Adapt a local-endpoint callback to an interface accepting a whole fragment.
-- Only the chosen branch is evaluated. This does not recurse into children.
fillInNested
    :: HasRouteLeaf fragment
    => (RouteLeaf fragment -> result)
    -> result
    -> fragment
    -> result
fillInNested onLeaf onNested = maybe onNested onLeaf . projectRouteLeaf

-- | Evidence that @fragment@ owns endpoints within @wholeRoute@, the full
-- route type (usually @Route site@). This identifies the fragment's type;
-- the actual endpoint and captures are carried separately by 'SomeRouteLeaf'.
--
-- For example, @FragmentAccountR :: RouteFragmentWitness (Route App) AccountR@.
-- A fragment can also be @Route App@ itself when the site has root endpoints.
-- No witness is generated for a fragment that only delegates.
data family RouteFragmentWitness wholeRoute fragment :: Type

-- | Generated once, polymorphic in @constraint@, with one constraint in its
-- context per endpoint-owning fragment. Concrete dictionaries are required
-- where this instance is used, not where route data is generated.
class RouteFragmentDict (constraint :: Type -> Constraint) wholeRoute where
    -- | Recover the chosen constraint for the fragment identified by a witness.
    getRouteFragmentDict :: RouteFragmentWitness wholeRoute fragment -> Dict (constraint fragment)

-- | The selected endpoint, including captures consumed by every ancestor.
data SomeRouteLeaf site where
    SomeRouteLeaf
        :: (HasRouteLeaf fragment, ParentSite fragment ~ site)
        => RouteFragmentWitness (Route site) fragment
        -> ParentArgs fragment
        -> RouteLeaf fragment
        -> SomeRouteLeaf site

-- | Project a matched route to its deepest endpoint-owning fragment.
-- Subsite mounts are leaves in the parent site; projection stops at the mount.
class RenderRoute site => RouteLeaves site where
    routeLeaf :: Route site -> SomeRouteLeaf site

-- | Visit the selected endpoint with a caller-chosen constraint. The callback
-- runs only for that endpoint, without visiting ancestors or falling back.
withRouteLeaf
    :: forall constraint site result.
       (RouteLeaves site, RouteFragmentDict constraint (Route site))
    => Route site
    -> (forall fragment.
           (HasRouteLeaf fragment, ParentSite fragment ~ site, constraint fragment)
           => ParentArgs fragment -> RouteLeaf fragment -> result)
    -> result
withRouteLeaf matched callback =
    case routeLeaf matched of
        SomeRouteLeaf witness args leaf ->
            case getRouteFragmentDict @constraint witness of
                Dict -> callback args leaf
