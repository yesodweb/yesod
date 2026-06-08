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
import YesodCoreTest.RuntimeHarness (assertGet)

mkYesodOpts (setNestedRouteFallthrough True defaultOpts) "FMApp" fmResources

instance Yesod FMApp where
    messageLoggerSource = mempty

getNeverFiresR :: HandlerFor FMApp String
getNeverFiresR = pure "NeverFiresR"

getOtherR :: HandlerFor FMApp String
getOtherR = pure "OtherR"

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
