{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Shared route table + foundation type for the "YesodCoreTest.NestedDispatch"
-- demo and its split-out parent modules. Lives in its own module so every
-- @setFocusOnNestedRoute@ splice (NestR, ParentR, Parent0R, ...) can refer to
-- the same @nestedDispatchResources@ tree without tripping the TH stage
-- restriction. The tree deliberately mixes flat routes, single- and
-- multi-piece dynamic parents, a multipiece (@*Texts@) leaf, and same-prefix
-- siblings to exercise the breadth of the nested-dispatch generator.
module YesodCoreTest.NestedDispatch.Resources where

import Yesod.Core
import Yesod.Routes.TH.Types

data App = App

nestedDispatchResources :: [ResourceTree String]
nestedDispatchResources = [parseRoutes|
/     HomeR GET !home
/json JsonR GET

/redirectparent RedirectParentR GET
/redirecttext   RedirectTextR   GET
/redirectstring RedirectStringR GET
/redirectparams RedirectParamsR GET
/nest NestR:
    / NestIndexR GET POST

/parent/#Int ParentR:
    /#Text/child1 Child1R !child
    /#Int/child2 Child2R !child GET POST

/parent0/#Int Parent0R:
    / Parent0IndexR GET

    /files/*Texts FilesR GET

    /child0/#Text Child0R:
        / ParentChildIndexR GET POST
        /#String ParentChildR GET POST

/nest OtherNestR:
    /foo NestFooR GET

/outer OuterR:
    /inner InnerR:
        / InnerIndexR GET !innerleaf

/blah BlahR:
    /   BlahIndexR GET

/robots.txt RobotsR:
    /   RobotsIndexR
|]
