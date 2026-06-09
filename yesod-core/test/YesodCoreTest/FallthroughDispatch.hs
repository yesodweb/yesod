{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module YesodCoreTest.FallthroughDispatch where

import YesodCoreTest.FallthroughDispatch.Resources
import YesodCoreTest.RuntimeHarness (assertGet)
import Yesod.Core
import Test.Hspec

mkYesodOpts (setNestedRouteFallthrough True defaultOpts) "App" fallthroughDispatchResources

instance Yesod App

handleFooIndexR :: HandlerFor app String
handleFooIndexR = pure "FooIndexR"

handleOtherR :: HandlerFor app String
handleOtherR = pure "OtherR"

handleFooNeatR :: HandlerFor app String
handleFooNeatR = pure "FooNeatR"

handleNeverFiresR :: HandlerFor app String
handleNeverFiresR = pure "NeverFiresR"

handleFooWithIntR :: Int -> HandlerFor app String
handleFooWithIntR i = pure $ "FooWithIntR " <> show i

handleFooWithTextR :: String -> HandlerFor app String
handleFooWithTextR str = pure $ "FooWithTextR " <> str

spec :: Spec
spec = do
    describe "FirstFooR" $ do
        it "FooIndexR" $ do
            assertGet App 200 ["foo"] (Just "FooIndexR")

        it "FooNeatR" $ do
            assertGet App 200 ["foo", "neat"] (Just "FooNeatR")

    describe "SecondFooR" $ do
        -- This is a little unsatisfying. Ideally we could write a test
        -- that expresses that, uh, `renderRoute (SecondFooR NeverFiresR)`
        -- would always direct to a different route. Difficult to do
        -- though.
        it "NeverFiresR" $ do
            assertGet App 200 ["foo"] (Just "FooIndexR")

        it "OtherR" $ do
            assertGet App 200 ["foo", "other"] (Just "OtherR")

    describe "ParamFooR" $ do
        it "FooWithIntR" $ do
            assertGet App 200 ["foo", "3"] (Just "FooWithIntR 3")

    describe "TextFooR" $ do
        it "FooWithTextR" $ do
            assertGet App 200 ["foo", "asdf"] (Just "FooWithTextR asdf")
