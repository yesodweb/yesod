{-# language TemplateHaskell #-}
{-# language ViewPatterns, OverloadedStrings #-}

module Hierarchy.Nest2.NestInner where

import Yesod.Routes.Parse
import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Language.Haskell.TH
--

mkRenderRouteInstanceOpts (setFocusOnNestedRoute (Just "NestInner") defaultOpts) [] (ConT ''Hierarchy) (map (fmap parseType) hierarchyResources)
mkRouteAttrsInstanceFor [] (ConT ''NestInner) "NestInner" $ map (fmap parseType) hierarchyResources
mkParseRouteInstanceFor "NestInner" $ map (fmap parseType) hierarchyResources
