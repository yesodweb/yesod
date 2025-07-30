{-# language TemplateHaskell #-}
{-# language OverloadedStrings, ViewPatterns, TemplateHaskell #-}
-- {-# options_ghc -ddump-splices #-}

module Hierarchy.Nest2 where

import Yesod.Routes.Parse
import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Language.Haskell.TH
import Hierarchy.Nest3
import Hierarchy.Nest2.NestInner

mkRenderRouteInstanceOpts (setFocusOnNestedRoute (Just "Nest2") defaultOpts) [] (ConT ''Hierarchy) (map (fmap parseType) hierarchyResources)
mkRouteAttrsInstanceFor [] (ConT ''Nest2) "Nest2" $ map (fmap parseType) hierarchyResources
mkParseRouteInstanceFor "Nest2" $ map (fmap parseType) hierarchyResources
