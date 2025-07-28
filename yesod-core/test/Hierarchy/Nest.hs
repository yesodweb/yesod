{-# language TemplateHaskell #-}

{-# OPTIONS_GHC -ddump-splices #-}

module Hierarchy.Nest where

import Yesod.Routes.Parse
import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Hierarchy.Nest3
import Hierarchy.Nest2
import Language.Haskell.TH

mkRenderRouteInstanceOpts (setFocusOnNestedRoute (Just "NestR") defaultOpts) [] (ConT ''Hierarchy) (map (fmap parseType) hierarchyResources)
mkRouteAttrsInstanceFor [] (ConT ''NestR) "NestR" $ map (fmap parseType) hierarchyResources
