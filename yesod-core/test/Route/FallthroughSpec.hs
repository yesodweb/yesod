{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns#-}


-- | The nested-route fallthrough contract for a top-level site, generated with
-- @setNestedRouteFallthrough True@. Two same-prefix parents share @\/foo@
-- (@FirstFooR@ index-only, @SecondFooR@ with deeper children). With fallthrough
-- on, a request missing inside @FirstFooR@ continues to the sibling rather than
-- committing. Asserts at both layers: @parseRoute@ falls through to the right
-- constructor, and a WAI dispatch round-trip (via 'YesodCoreTest.RuntimeHarness')
-- serves the expected handler.
module Route.FallthroughSpec where

import Network.Wai
import Yesod.Core
import Test.Hspec
import Data.ByteString.Lazy (ByteString)

import YesodCoreTest.RuntimeHarness (assertRequestRaw)

data App = App

do
    let resources = [parseRoutesNoCheck|

/ HomeR
/foo FirstFooR:
    /   FooIndexR
/foo SecondFooR:
    /blah FooBlahR
    /baz  FooBaz1R:
        /   FooBazIndexR
    /baz    FooBaz2R:
        /foo    FooBaz2FooR

    |]

    let opts = setNestedRouteFallthrough True defaultOpts
    mkYesodOpts opts "App" resources

instance Yesod App where
    messageLoggerSource = mempty

handleFooBlahR :: HandlerFor site String
handleFooBlahR = pure "FooBlahR"

handleHomeR :: HandlerFor site String
handleHomeR = pure "HomeR"

handleFooBaz2FooR :: HandlerFor site String
handleFooBaz2FooR = pure "FooBaz2FooR"

handleFooBazIndexR :: HandlerFor site String
handleFooBazIndexR = pure "FooBazIndexR"

handleFooIndexR :: HandlerFor site String
handleFooIndexR = pure "FooIndexR"

spec :: Spec
spec = do
    describe "parseRoute" $ do
        let routeShouldParse path result =
                parseRoute (path, []) `shouldBe` Just result
            routeShouldNotParse path =
                parseRoute (path, []) `shouldBe` (Nothing :: Maybe (Route App))
        it "matches the first parent without falling through" $ do
            routeShouldParse ["foo"] (FirstFooR FooIndexR)
        it "can fall through" $ do
            routeShouldParse ["foo", "blah"] (SecondFooR FooBlahR)
        it "nested fallthrough works too" $ do
            routeShouldParse ["foo", "baz", "foo"] (SecondFooR (FooBaz2R FooBaz2FooR))
        it "can fail" $ do
            routeShouldNotParse ["asdf"]

    describe "Dispatch" $ do
        it "/" $ do
            testRequestIO
                200
                defaultRequest
                    { pathInfo = []
                    }
                (Just "HomeR")
        it "/foo" $ do
            testRequestIO
                200
                defaultRequest
                    { pathInfo = ["foo"]
                    }
                (Just "FooIndexR")

testRequestIO :: HasCallStack => Int -- ^ http status code
            -> Request
            -> Maybe ByteString -- ^ expected body
            -> IO ()
testRequestIO status req mexpected =
    assertRequestRaw (toWaiApp App) req status mexpected
