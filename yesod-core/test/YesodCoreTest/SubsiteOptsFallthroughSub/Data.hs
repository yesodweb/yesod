{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Subsite route data for the 'mkYesodSubDispatchInstanceOpts' fallthrough
-- runtime test. Unlike "YesodCoreTest.SubsiteFallthroughSub" (whose sibling
-- parents live one level down inside @WrapR@), these subsites place two
-- same-prefix parents at the subsite's *own top level* — the exact seam that
-- the @yesodSubDispatch@ body's inline parent clauses govern. @FirstFooR@
-- matches only an empty suffix (its index); @SecondFooR@ matches @\/bar@. A
-- request for @\/foo\/bar@ misses inside @FirstFooR@ and can only reach
-- @SecondFooR@ by falling through — the behaviour that
-- 'mkYesodSubDispatchInstanceOpts' must now thread into the body via
-- 'setNestedRouteFallthrough'.
--
-- The route data lives apart from the dispatch splice (which goes in the test
-- module) because the generated @resources*@ binding can't feed a splice in
-- the same module (GHC stage restriction).
module YesodCoreTest.SubsiteOptsFallthroughSub.Data where

import Yesod.Core

data OptsOnSub = OptsOnSub

mkYesodSubData "OptsOnSub" [parseRoutes|
/foo OnFirstFooR:
    / OnFooIndexR GET
/foo OnSecondFooR:
    /bar OnFooBarR GET
|]

data OptsOffSub = OptsOffSub

mkYesodSubData "OptsOffSub" [parseRoutes|
/foo OffFirstFooR:
    / OffFooIndexR GET
/foo OffSecondFooR:
    /bar OffFooBarR GET
|]
