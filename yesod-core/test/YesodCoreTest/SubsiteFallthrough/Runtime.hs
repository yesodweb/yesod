{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Runtime (WAI) coverage for subsite nested-route fallthrough. Every other
-- 'setNestedRouteFallthrough' fixture is a top-level site; this mounts two
-- subsites — one with fallthrough enabled, one without — and proves that a
-- request which misses inside a nested parent reaches the sibling route only
-- when the subsite was generated with fallthrough on. See
-- "YesodCoreTest.SubsiteFallthrough.Data" for the subsite definitions.
module YesodCoreTest.SubsiteFallthrough.Runtime (specs) where

import Test.Hspec
import Yesod.Core

import YesodCoreTest.SubsiteFallthrough.Data
-- Bring the split-out YesodSubDispatchNested instances into scope (instances
-- only) so the host site's mkYesodSubDispatch delegates WrapR to them.
import YesodCoreTest.SubsiteFallthrough.Nested ()
import YesodCoreTest.RuntimeHarness (assertGet)

data FallApp = FallApp

getOnSub :: FallApp -> FallOnSub
getOnSub _ = FallOnSub

getOffSub :: FallApp -> FallOffSub
getOffSub _ = FallOffSub

mkYesod "FallApp" [parseRoutes|
/on  OnSubR  FallOnSub  getOnSub
/off OffSubR FallOffSub getOffSub
|]

instance Yesod FallApp

-- The host site delegates each subsite's nested @WrapR@ to the
-- 'YesodSubDispatchNested' instance imported from
-- "YesodCoreTest.SubsiteFallthrough.Data".
instance YesodSubDispatch FallOnSub master where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesFallOnSub)

instance YesodSubDispatch FallOffSub master where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesFallOffSub)

specs :: Spec
specs = describe "YesodCoreTest.SubsiteFallthrough.Runtime (subsite nested-route fallthrough)" $ do
    describe "fallthrough enabled" $ do
        it "still reaches the shadowing nested index route" $
            assertGet FallApp 200 ["on", "wrap", "foo"] (Just "onFooAIndex")
        it "falls through a nested miss to the sibling route" $
            assertGet FallApp 200 ["on", "wrap", "foo", "bar"] (Just "onFooBBar")

    describe "fallthrough disabled (default)" $ do
        it "still reaches the shadowing nested index route" $
            assertGet FallApp 200 ["off", "wrap", "foo"] (Just "offFooAIndex")
        it "404s after a nested miss instead of falling through to the sibling" $
            assertGet FallApp 404 ["off", "wrap", "foo", "bar"] Nothing
