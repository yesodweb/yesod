{-# language FlexibleContexts #-}

module Yesod.Core.Class.Dispatch.ToParentRoute where

import Yesod.Routes.Class

-- | Reconstruct a full parent route from a nested route fragment and its
-- 'ParentArgs' (the dynamic pieces consumed by the ancestor parents). This is
-- what lets a fragment dispatched in its own module be rendered back into the
-- master site's @'Route' ('ParentSite' a)@.
--
-- @since 1.7.0.0
class (RenderRoute (ParentSite a)) => ToParentRoute a where
    -- | @since 1.7.0.0
    toParentRoute :: ParentArgs a -> a -> Route (ParentSite a)

instance (RenderRoute a) => ToParentRoute (Route a) where
    toParentRoute _ = id

-- | Recover a fragment and its ancestor captures from a full route. Generated
-- for mount-owning fragments alongside local leaf views in the root data
-- splice, without policy constraints.
-- A route belonging to another fragment returns 'Nothing'.
--
-- @since 1.7.1.0
class ToParentRoute a => FromParentRoute a where
    fromParentRoute :: Route (ParentSite a) -> Maybe (WithParentArgs a)

instance RenderRoute a => FromParentRoute (Route a) where
    fromParentRoute = Just . WithParentArgs ()
