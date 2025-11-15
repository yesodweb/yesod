{-# language TemplateHaskell, ViewPatterns, OverloadedStrings #-}

{-# OPTIONS_GHC -ddump-splices #-}

module Hierarchy.Admin where

import Yesod.Routes.Parse
import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Language.Haskell.TH
import Data.Text (Text)

mkRenderRouteInstanceOpts
    (setFocusOnNestedRoute (Just "AdminR") defaultOpts)
    []
    []
    (ConT ''Hierarchy)
    (map (fmap parseType) hierarchyResources)
mkRouteAttrsInstanceFor
    []
    (ConT ''AdminR)
    "AdminR"
    $ map (fmap parseType) hierarchyResources
mkParseRouteInstanceFor
    "AdminR"
    $ map (fmap parseType) hierarchyResources
