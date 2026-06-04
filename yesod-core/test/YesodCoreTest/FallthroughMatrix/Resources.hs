{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Shared route table + site type for the cross-module fallthrough matrix.
--
-- Two parents share the @\/foo@ prefix: @FirstFooR@ (whose nested instance is
-- generated in a /separate/ module, "YesodCoreTest.FallthroughMatrix.FirstFoo",
-- with the default @fallthrough = False@) and @SecondFooR@ (inlined in the
-- top module "YesodCoreTest.FallthroughMatrixRuntime", which sets
-- @fallthrough = True@). The flag mismatch across the module split is the whole
-- point: it lets us pin who owns the terminal 404 on @\/foo\/other@.
module YesodCoreTest.FallthroughMatrix.Resources where

import Yesod.Core
import Yesod.Routes.TH.Types

data FMApp = FMApp

fmResources :: [ResourceTree String]
fmResources = [parseRoutesNoCheck|
/foo   FirstFooR:
    /      FooIndexR  GET
    /neat  FooNeatR   GET

/foo   SecondFooR:
    /       NeverFiresR  GET
    /other  OtherR       GET
|]
