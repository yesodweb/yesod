{-# language TemplateHaskell #-}
{-# language ViewPatterns, OverloadedStrings #-}
{-# language TypeFamilies #-}

module Hierarchy.Nest2.NestInner where

import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Language.Haskell.TH

mkRenderRouteInstanceOpts
    (setFocusOnNestedRoute (Just "NestInner") defaultOpts)
    []
    NoTyArgs
    (ConT ''Hierarchy)
    hierarchyResourcesWithType
mkRouteAttrsInstanceFor [] (ConT ''NestInner) "NestInner" $ hierarchyResourcesWithType
mkParseRouteInstanceFor "NestInner" $ hierarchyResourcesWithType
