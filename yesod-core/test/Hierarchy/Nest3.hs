{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}
-- {-# options_ghc -ddump-splices #-}

module Hierarchy.Nest3 where

import Yesod.Routes.Parse
import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Language.Haskell.TH

mkRenderRouteInstanceOpts (setFocusOnNestedRoute (Just "Nest3") defaultOpts) [] (ConT ''Hierarchy) (map (fmap parseType) hierarchyResources)
mkRouteAttrsInstanceFor [] (ConT ''Nest3) "Nest3" $ map (fmap parseType) hierarchyResources
mkParseRouteInstanceFor "Nest3" $ map (fmap parseType) hierarchyResources
