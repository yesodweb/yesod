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

{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Route.FallthroughSpec where

import Network.Wai
import Network.Wai.Test
import Yesod.Core
import Test.Hspec
import Data.Foldable (for_)
import Data.ByteString.Lazy (ByteString)
import Test.Hspec.Expectations.Contrib (annotate)
import qualified Network.HTTP.Types as H

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

testRequestIO :: HasCallStack => Int -- ^ http status code
            -> Request
            -> Maybe ByteString -- ^ expected body
            -> IO ()
testRequestIO status req mexpected = do
    app <- toWaiApp App
    sres <- flip runSession app $ do
        request req
    annotate ("Request body: " <> show (simpleBody sres )) $ do
        H.statusCode (simpleStatus sres) `shouldBe` status
        for_ mexpected $ \expected -> do
            simpleBody sres `shouldBe` expected
