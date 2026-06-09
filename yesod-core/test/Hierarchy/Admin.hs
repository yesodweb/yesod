{-# language TemplateHaskell, ViewPatterns, OverloadedStrings, TypeFamilies #-}

-- | @Hierarchy@ split fragment focused on the @AdminR@ parent. Shows the
-- finer-grained helpers — @mkRenderRouteInstanceOpts@ /
-- @mkRouteAttrsInstanceFor@ / @mkParseRouteInstanceFor@ each scoped to
-- @"AdminR"@ — emitting that one fragment's instances against the shared
-- @hierarchyResourcesWithType@ tree, rather than the whole-tree splice in
-- "Hierarchy".
module Hierarchy.Admin where

import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Language.Haskell.TH
import Data.Text (Text)

mkRenderRouteInstanceOpts
    (setFocusOnNestedRoute "AdminR" defaultOpts)
    []
    NoTyArgs
    (ConT ''Hierarchy)
    hierarchyResourcesWithType
mkRouteAttrsInstanceFor
    []
    (ConT ''AdminR)
    "AdminR"
    $ hierarchyResourcesWithType
mkParseRouteInstanceFor
    "AdminR"
    $ hierarchyResourcesWithType
