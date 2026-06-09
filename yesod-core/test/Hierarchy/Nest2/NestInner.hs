{-# language TemplateHaskell #-}
{-# language ViewPatterns, OverloadedStrings #-}
{-# language TypeFamilies #-}

-- | @Hierarchy@ split fragment focused on the deepest nested parent
-- @NestInner@ (under @NestR@ > @Nest2@). Emits its render\/attrs\/parse
-- instances via @setFocusOnNestedRoute "NestInner"@ + the @*For@ helpers; the
-- @ParseRouteNested NestInner@ instance is what "Hierarchy.Nest2" asserts
-- against at compile time.
module Hierarchy.Nest2.NestInner where

import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Language.Haskell.TH

mkRenderRouteInstanceOpts
    (setFocusOnNestedRoute "NestInner" defaultOpts)
    []
    NoTyArgs
    (ConT ''Hierarchy)
    hierarchyResourcesWithType
mkRouteAttrsInstanceFor [] (ConT ''NestInner) "NestInner" $ hierarchyResourcesWithType
mkParseRouteInstanceFor "NestInner" $ hierarchyResourcesWithType
