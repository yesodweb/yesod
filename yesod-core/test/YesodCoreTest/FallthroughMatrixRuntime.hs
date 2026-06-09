{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | The cross-module fallthrough flag-mismatch cell of the T5 matrix.
--
-- This top module sets @fallthrough = True@ and delegates @FirstFooR@ to the
-- nested instance generated (with the default @fallthrough = False@) in
-- "YesodCoreTest.FallthroughMatrix.FirstFoo". @SecondFooR@ is inlined here.
--
-- The single-module analogue "YesodCoreTest.ParamFallthroughRuntime" proves
-- that with fallthrough enabled, @\/foo\/other@ falls through @FirstFooR@ (which
-- has no @other@ child) to @SecondFooR@ → @OtherR@ (200). This module is the
-- same shape but with @FirstFooR@'s instance split into a module compiled with
-- the opposite flag — exactly the configuration the R10 fix targets.
module YesodCoreTest.FallthroughMatrixRuntime
    ( specs
    ) where

import Test.Hspec
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Yesod.Core

import YesodCoreTest.FallthroughMatrix.Resources
import YesodCoreTest.FallthroughMatrix.FirstFoo (FirstFooR(..))
import YesodCoreTest.RuntimeHarness (assertGet, assertRequestFor)

mkYesodOpts (setNestedRouteFallthrough True defaultOpts) "FMApp" fmResources

instance Yesod FMApp where
    messageLoggerSource = mempty

getNeverFiresR :: HandlerFor FMApp String
getNeverFiresR = pure "NeverFiresR"

getOtherR :: HandlerFor FMApp String
getOtherR = pure "OtherR"

postNeatPostR :: HandlerFor FMApp String
postNeatPostR = pure "NeatPostR"

testRequestIO :: HasCallStack => Int -> [Text] -> Maybe ByteString -> IO ()
testRequestIO = assertGet FMApp

specs :: Spec
specs = describe "cross-module fallthrough flag mismatch (top True, split child False)" $ do
    it "matches the split child's index (FooIndexR at /foo)" $
        testRequestIO 200 ["foo"] (Just "FooIndexR")
    it "matches a child of the split parent (FooNeatR at /foo/neat)" $
        testRequestIO 200 ["foo", "neat"] (Just "FooNeatR")
    it "falls through the split parent to the inline sibling (OtherR at /foo/other)" $
        -- The headline R10 case. The top module wants fallthrough; FirstFooR
        -- has no "other" child. Post-R10 this falls through to SecondFooR's
        -- OtherR (200). Pre-R10, FirstFooR's split instance (flag False) bakes
        -- a 404 and defeats the parent's fallthrough.
        testRequestIO 200 ["foo", "other"] (Just "OtherR")

    -- Terminal semantics of fallthrough (ISSUE 11). Fallthrough is triggered
    -- ONLY by a path-level miss (a 'Nothing' from the matched parent): a
    -- matched leaf — even one rejecting the method — returns @Just <405>@, so
    -- dispatch commits to that leaf and never falls through to a same-prefix
    -- sibling. These rows pin both halves of that contract.
    describe "wrong method on a matched leaf commits (does not fall through)" $ do
        it "POST /foo/neat commits to 405 at FirstFooR's GET leaf" $
            -- FirstFooR owns /foo/neat as GET-only and SecondFooR owns the same
            -- /foo/neat path as POST-only. A POST matches FirstFooR's leaf by
            -- PATH, so the split (fallthrough=False) FirstFooR instance returns
            -- Just <405> — it does NOT return Nothing — and the host's
            -- fallthrough arm therefore commits to 405 rather than falling
            -- through to SecondFooR's POST leaf (which would be 200). If this
            -- ever flips to 200, fallthrough has started swallowing a matched
            -- leaf's method rejection, which is a contract change.
            assertRequestFor FMApp "POST" 405 ["foo", "neat"] Nothing
        it "GET /foo/neat still serves FirstFooR's GET leaf (200)" $
            -- The mirror of the above: the same shared path under the right
            -- method resolves to the first matching leaf, never SecondFooR.
            testRequestIO 200 ["foo", "neat"] (Just "FooNeatR")

    describe "total miss under fallthrough lands on the terminal 404" $ do
        it "GET /foo/zzz exhausts both same-prefix parents -> 404" $
            -- /foo/zzz matches the /foo prefix of both FirstFooR and SecondFooR
            -- but no leaf of either. Each parent reports a path miss ('Nothing'),
            -- so dispatch falls through every sibling and lands on the terminal
            -- 404 clause ("except in the final case" from the flag's haddock).
            testRequestIO 404 ["foo", "zzz"] Nothing
        it "GET /zzz matches no parent prefix at all -> 404" $
            -- A top-level total miss: no parent's prefix matches, so the only
            -- clause that fires is the terminal 404.
            testRequestIO 404 ["zzz"] Nothing
