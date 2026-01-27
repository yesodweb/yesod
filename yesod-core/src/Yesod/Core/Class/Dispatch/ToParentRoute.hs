{-# language FlexibleContexts #-}

module Yesod.Core.Class.Dispatch.ToParentRoute where

import Yesod.Routes.Class

class (RenderRoute (ParentSite a)) => ToParentRoute a where
    toParentRoute :: ParentArgs a -> a -> Route (ParentSite a)

instance (RenderRoute a) => ToParentRoute (Route a) where
    toParentRoute _ = id
