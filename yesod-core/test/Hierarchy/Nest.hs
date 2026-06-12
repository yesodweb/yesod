{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}

-- | @Hierarchy@ split fragment focused on the top-level nested parent @NestR@
-- (which itself contains @Nest2@\/@Nest3@). Emits @NestR@'s render\/attrs\/parse
-- instances via the @*For "NestR"@ helpers against the shared
-- @hierarchyResourcesWithType@. Imports the deeper @Nest2@\/@Nest3@\/@NestInner@
-- fragment modules so their separately-emitted instances are in scope.
--
-- The three splices below are the canonical low-level recipe for emitting one
-- fragment's instances when splitting route compilation across modules: a
-- focused 'mkRenderRouteInstanceOpts' plus the @*For@ attrs\/parse splices,
-- all against the one shared resource tree. A full Yesod site gets the same
-- effect from the higher-level @mkYesodOpts (setFocusOnNestedRoute \"NestR\" ...)@
-- — see @yesod-core\/docs\/split-route-compilation.md@; this suite calls the
-- route-level generators directly because it has no dispatch layer.
module Hierarchy.Nest where

import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Hierarchy.Nest3 ()
import Hierarchy.Nest2
import Language.Haskell.TH
import Hierarchy.Nest2.NestInner

mkRenderRouteInstanceOpts (setFocusOnNestedRoute "NestR" defaultOpts) [] NoTyArgs (ConT ''Hierarchy) hierarchyResourcesWithType
mkRouteAttrsInstanceFor [] (ConT ''NestR) "NestR" $ hierarchyResourcesWithType
mkParseRouteInstanceFor "NestR" $ hierarchyResourcesWithType
