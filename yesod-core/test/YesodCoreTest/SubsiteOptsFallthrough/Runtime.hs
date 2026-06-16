{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Runtime (WAI) coverage for 'mkYesodSubDispatchInstanceOpts' with
-- 'setNestedRouteFallthrough'. The headline use case of that entry point —
-- a subsite whose own top-level parent clauses fall through to a later sibling
-- on an inner miss — was previously a no-op: the generated @yesodSubDispatch@
-- body came from the opts-less 'mkYesodSubDispatch', which hardcoded
-- @mdsNestedRouteFallthrough = False@. This pins the fix.
--
-- Two subsites are mounted: 'OptsOnSub' generated with fallthrough on,
-- 'OptsOffSub' at the default. Both have two same-prefix top-level parents
-- (@\/foo FirstFooR@ then @\/foo SecondFooR@); @\/foo\/bar@ reaches the
-- @SecondFooR@ sibling only when the body honours the flag. See
-- "YesodCoreTest.SubsiteOptsFallthrough.Data" for the route data.
module YesodCoreTest.SubsiteOptsFallthrough.Runtime (specs) where

import Data.Text (Text)
import Test.Hspec
import Yesod.Core
import Yesod.Core.Dispatch (mkYesodSubDispatchInstanceOpts)

import YesodCoreTest.SubsiteOptsFallthrough.Data
import YesodCoreTest.RuntimeHarness (assertGet)

-- Handlers for the fallthrough-ENABLED subsite.
getOnFooIndexR :: SubHandlerFor OptsOnSub master Text
getOnFooIndexR = pure "onFooIndex"

getOnFooBarR :: SubHandlerFor OptsOnSub master Text
getOnFooBarR = pure "onFooBar"

-- Handlers for the fallthrough-DISABLED subsite.
getOffFooIndexR :: SubHandlerFor OptsOffSub master Text
getOffFooIndexR = pure "offFooIndex"

getOffFooBarR :: SubHandlerFor OptsOffSub master Text
getOffFooBarR = pure "offFooBar"

-- The API under test: the @yesodSubDispatch@ body must thread the fallthrough
-- flag from these opts so the subsite's own top-level @FirstFooR@/@SecondFooR@
-- siblings fall through. Placed before 'mkYesod' so the host dispatch sees the
-- generated subsite instances.
mkYesodSubDispatchInstanceOpts
    (setNestedRouteFallthrough True defaultOpts)
    "OptsOnSub"
    resourcesOptsOnSub

mkYesodSubDispatchInstanceOpts
    defaultOpts
    "OptsOffSub"
    resourcesOptsOffSub

data OptsFallApp = OptsFallApp

getOnSub :: OptsFallApp -> OptsOnSub
getOnSub _ = OptsOnSub

getOffSub :: OptsFallApp -> OptsOffSub
getOffSub _ = OptsOffSub

mkYesod "OptsFallApp" [parseRoutes|
/on  OnSubR  OptsOnSub  getOnSub
/off OffSubR OptsOffSub getOffSub
|]

instance Yesod OptsFallApp

specs :: Spec
specs = describe "YesodCoreTest.SubsiteOptsFallthrough.Runtime (mkYesodSubDispatchInstanceOpts fallthrough)" $ do
    describe "fallthrough enabled (setNestedRouteFallthrough True)" $ do
        it "still reaches the shadowing top-level index route" $
            assertGet OptsFallApp 200 ["on", "foo"] (Just "onFooIndex")
        it "falls through a top-level parent miss to the sibling route" $
            assertGet OptsFallApp 200 ["on", "foo", "bar"] (Just "onFooBar")

    describe "fallthrough disabled (default)" $ do
        it "still reaches the shadowing top-level index route" $
            assertGet OptsFallApp 200 ["off", "foo"] (Just "offFooIndex")
        it "404s after a top-level parent miss instead of falling through" $
            assertGet OptsFallApp 404 ["off", "foo", "bar"] Nothing
