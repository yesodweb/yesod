{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}

module Hierarchy.Nest where

import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Hierarchy.Nest3 ()
import Hierarchy.Nest2
import Language.Haskell.TH
import Hierarchy.Nest2.NestInner

mkRenderRouteInstanceOpts (setFocusOnNestedRoute (Just "NestR") defaultOpts) [] NoTyArgs (ConT ''Hierarchy) hierarchyResourcesWithType
mkRouteAttrsInstanceFor [] (ConT ''NestR) "NestR" $ hierarchyResourcesWithType
mkParseRouteInstanceFor "NestR" $ hierarchyResourcesWithType
