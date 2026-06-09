{-# language TemplateHaskell, OverloadedStrings, ViewPatterns, TypeFamilies #-}

-- | @Hierarchy@ split fragment focused on the @Nest2@ parent. Emits @Nest2@'s
-- render\/attrs\/parse instances against the shared @hierarchyResourcesWithType@.
-- The top-of-module @do@ block is a compile-time assertion: it calls
-- @parseRouteNested@ for the deeper @NestInner@ fragment (whose instance is
-- emitted in "Hierarchy.Nest2.NestInner") and @fail@s the splice if parsing the
-- empty path does not yield @NestInnerIndexR@ — pinning cross-module nested parse.
module Hierarchy.Nest2 where

import Yesod.Routes.Class
import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Language.Haskell.TH
import Hierarchy.Nest3 ()
import Hierarchy.Nest2.NestInner

do
    let works = parseRouteNested ([], []) :: Maybe NestInner
    case works of
        Nothing ->
            fail "parsing fails"
        Just NestInnerIndexR ->
            pure []

mkRenderRouteInstanceOpts (setFocusOnNestedRoute "Nest2" defaultOpts) [] NoTyArgs (ConT ''Hierarchy) hierarchyResourcesWithType
mkRouteAttrsInstanceFor [] (ConT ''Nest2) "Nest2" $ hierarchyResourcesWithType
mkParseRouteInstanceFor "Nest2" $ hierarchyResourcesWithType
