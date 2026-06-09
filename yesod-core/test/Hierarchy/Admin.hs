{-# language TemplateHaskell, ViewPatterns, OverloadedStrings, TypeFamilies #-}

module Hierarchy.Admin where

import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Language.Haskell.TH
import Data.Text (Text)

mkRenderRouteInstanceOpts
    (setFocusOnNestedRoute (Just "AdminR") defaultOpts)
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
