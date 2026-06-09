{-# language TemplateHaskell, OverloadedStrings, ViewPatterns, TypeFamilies #-}

module Hierarchy.Nest2 where

import Yesod.Routes.Class
import Yesod.Routes.TH
import Hierarchy.ResourceTree
import Language.Haskell.TH
import Hierarchy.Nest3 ()
import Hierarchy.Nest2.NestInner

do
    let works = parseRouteNested ([], []) :: Maybe NestInner
    case works of
        Nothing ->
            fail "parsing fails"
        Just NestInnerIndexR ->
            pure []

mkRenderRouteInstanceOpts (setFocusOnNestedRoute (Just "Nest2") defaultOpts) [] NoTyArgs (ConT ''Hierarchy) hierarchyResourcesWithType
mkRouteAttrsInstanceFor [] (ConT ''Nest2) "Nest2" $ hierarchyResourcesWithType
mkParseRouteInstanceFor "Nest2" $ hierarchyResourcesWithType
