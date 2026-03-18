{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Route.RenderRouteSpec where

import Yesod.Core
import Test.Hspec

data App = App

mkYesodData "App" [parseRoutes|

/foo/#Int   FooR:
    /   FooIndexR
    /bar/#String    FooBarR:
        /   FooBarIndexR
        /baz    FooBarBazR:
            /new    FooBarBazNewR
            /delete/#String FooBarBazDeleteR
            /get/#String    FooBarBazGetR

    |]

spec :: Spec
spec = do
    describe "renderRouteNested" $ do
        it "renders the entire route, not just end fragment" $ do
            renderRouteNested (3, "hello") (FooBarBazGetR "asdf")
                `shouldBe`
                    (["foo", "3", "bar", "hello", "baz", "get", "asdf"], [])
