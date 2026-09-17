{-# LANGUAGE TypeFamilies #-}

-- | Structural views of the endpoints directly owned by a route fragment.
-- Generated with @setRouteLeafViews True@ or @setRouteLeafHandlerWrapper@.
module Yesod.Routes.Class.Leaf
    ( HasRouteLeaves (..)
    , fillInNested
    ) where

import Data.Kind (Type)
import Yesod.Routes.Class

-- | A fragment's direct endpoints, excluding delegation constructors.
-- Purely delegating fragments do not need this instance.
class RenderRouteNested fragment => HasRouteLeaves fragment where
    -- | Direct endpoint constructors and their captures, with delegation
    -- constructors omitted. Generated constructor names start with @Leaf@.
    data RouteLeaves fragment :: Type
    -- | A shallow projection. 'Nothing' means a delegation constructor.
    projectRouteLeaves :: fragment -> Maybe (RouteLeaves fragment)
    -- | Embed a local endpoint back into its original fragment.
    -- Projecting this result must return the original leaf value.
    fromRouteLeaves :: RouteLeaves fragment -> fragment

-- | Adapt a local-endpoint callback to an interface accepting a whole fragment.
-- Only the chosen branch is evaluated. This does not recurse into children.
fillInNested
    :: HasRouteLeaves fragment
    => (RouteLeaves fragment -> result)
    -> result
    -> fragment
    -> result
fillInNested onLeaf onNested = maybe onNested onLeaf . projectRouteLeaves

