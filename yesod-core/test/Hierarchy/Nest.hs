{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}

module Hierarchy.Nest where

import Yesod.Routes.Parse
import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Hierarchy.Nest3
import Hierarchy.Nest2
import Language.Haskell.TH
import Hierarchy.Nest2.NestInner

mkRenderRouteInstanceOpts (setFocusOnNestedRoute (Just "NestR") defaultOpts) [] [] (ConT ''Hierarchy) (map (fmap parseType) hierarchyResources)
mkRouteAttrsInstanceFor [] (ConT ''NestR) "NestR" $ map (fmap parseType) hierarchyResources
mkParseRouteInstanceFor "NestR" $ map (fmap parseType) hierarchyResources
