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
