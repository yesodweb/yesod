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

-- | Behavioural spec for @renderRouteNested@ on a deeply-nested route tree.
-- Pins the contract that rendering a nested leaf reconstructs the /entire/ path
-- from the site root (parent pieces and captures included), not just the leaf's
-- own fragment — the caller supplies the ancestor captures as a tuple.
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
