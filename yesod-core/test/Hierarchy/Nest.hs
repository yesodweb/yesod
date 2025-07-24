{-# language TemplateHaskell #-}
{-# options_ghc -ddump-deriv #-}

module Hierarchy.Nest where

import Yesod.Routes.Parse
import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Hierarchy.Nest3
import Hierarchy.Nest2
import Language.Haskell.TH

mkRenderRouteInstanceOpts (setFocusOnNestedRoute (Just "Nest") defaultOpts) [] (ConT ''Hierarchy) (map (fmap parseType) hierarchyResources)
