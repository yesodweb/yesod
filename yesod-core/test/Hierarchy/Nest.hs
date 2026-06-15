{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}

-- | @Hierarchy@ split fragment focused on the top-level nested parent @NestR@
-- (which itself contains @Nest2@\/@Nest3@). Uses the canonical
-- @mkYesodDataOpts@ with @setFocusOnNestedRoute "NestR"@ to emit just that
-- fragment's route datatype + render\/attrs\/parse instances — the same
-- higher-level entry point a full Yesod site uses to split route compilation
-- across modules (see @yesod-core\/docs\/split-route-compilation.md@). Imports
-- the deeper @Nest2@\/@Nest3@\/@NestInner@ fragment modules so their
-- separately-emitted instances are in scope for @NestR@ to delegate to.
--
-- (Sibling fragments "Hierarchy.Admin", "Hierarchy.Nest2" and
-- "Hierarchy.Nest2.NestInner" instead drive the underlying
-- @mkRenderRouteInstanceOpts@ \/ @mkRouteAttrsInstanceFor@ \/
-- @mkParseRouteInstanceFor@ builders directly, so the route-level generators
-- that @mkYesodDataOpts@ bundles stay covered too.)
module Hierarchy.Nest where

import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Hierarchy.Nest3 ()
import Hierarchy.Nest2
import Hierarchy.Nest2.NestInner
import Yesod.Core

mkYesodDataOpts (setFocusOnNestedRoute "NestR" defaultOpts) "Hierarchy" hierarchyResources
