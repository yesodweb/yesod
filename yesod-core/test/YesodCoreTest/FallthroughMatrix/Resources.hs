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
--
-- @FirstFooR@ owns @\/foo\/neat@ as @GET@-only; @SecondFooR@ owns the same
-- @\/foo\/neat@ path as @POST@-only. This shared @neat@ leaf is what pins the
-- wrong-method contract under fallthrough: a @POST \/foo\/neat@ matches
-- @FirstFooR@'s @GET@ leaf /by path/ and so must commit to 405 — fallthrough is
-- only triggered by a path miss ('Nothing'), and a matched-path/wrong-method
-- leaf returns @Just \<405\>@, never 'Nothing'. If it instead fell through to
-- @SecondFooR@'s @POST@ leaf it would 200, which would be a contract change.
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
    /neat   NeatPostR    POST
|]
