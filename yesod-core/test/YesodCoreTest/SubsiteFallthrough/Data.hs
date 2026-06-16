{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Subsite route data for the nested-route fallthrough runtime test. Both
-- subsites have a nested parent @WrapR@ containing two sibling sub-parents that
-- share the @\/foo@ prefix: the first (@FooAR@) matches only an empty suffix
-- (its index), the second (@FooBR@) matches @\/bar@. A request for
-- @\/wrap\/foo\/bar@ therefore misses in @FooAR@ and can only reach @FooBR@ by
-- falling through — the behaviour the dispatch modules toggle.
--
-- The route data lives here, apart from the 'mkNestedSubDispatchInstance'
-- splices in "YesodCoreTest.SubsiteFallthrough.Nested", because the
-- generated @resources*@ binding can't feed a splice in the same module (GHC
-- stage restriction) — the same split "YesodCoreTest.SplitSubsite.Runtime" uses.
module YesodCoreTest.SubsiteFallthrough.Data where

import Yesod.Core

data FallOnSub = FallOnSub

mkYesodSubData "FallOnSub" [parseRoutes|
/wrap OnWrapR:
    /foo OnFooAR:
        / OnFooAIndexR GET
    /foo OnFooBR:
        /bar OnFooBBarR GET
|]

data FallOffSub = FallOffSub

mkYesodSubData "FallOffSub" [parseRoutes|
/wrap OffWrapR:
    /foo OffFooAR:
        / OffFooAIndexR GET
    /foo OffFooBR:
        /bar OffFooBBarR GET
|]
