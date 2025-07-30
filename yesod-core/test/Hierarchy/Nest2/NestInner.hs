{-# language TemplateHaskell #-}
{-# language ViewPatterns, OverloadedStrings #-}
{-# options_ghc -ddump-splices -ddump-to-file #-}

module Hierarchy.Nest2.NestInner where

import Yesod.Routes.Parse
import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Language.Haskell.TH
import Hierarchy.Nest3

mkRenderRouteInstanceOpts (setFocusOnNestedRoute (Just "NestInner") defaultOpts) [] (ConT ''Hierarchy) (map (fmap parseType) hierarchyResources)
mkRouteAttrsInstanceFor [] (ConT ''NestInner) "NestInner" $ map (fmap parseType) hierarchyResources
mkParseRouteInstanceFor "NestInner" $ map (fmap parseType) hierarchyResources
