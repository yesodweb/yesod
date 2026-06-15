{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Exercises dispatch-supplied route authorization ('RouteAuthPerResource').
--
-- The site below opts every leaf into a demanded @authorize\<Resource\>@
-- binding via 'setRouteAuthorization'. This covers both injection points:
--
--   * @OpenR@\/@SecretR@ are top-level leaves, reached through the flat
--     dispatch clause ('mkDispatchClause').
--   * @InnerR@ is a leaf under the @SubR@ parent. A monomorphic site uses
--     'NestedDiscovery', so @SubR@'s @YesodDispatchNested@ instance is
--     generated in this same module and @InnerR@ is reached through
--     'genNestedDispatchClauses'.
--
-- 'isAuthorized' is overridden to authorize everything, so any 403 below can
-- only come from the dispatch-supplied authorizer glued onto the handler —
-- and a 403 on a method-mismatch request proves the glued check runs before
-- the 405 is revealed, exactly as 'isAuthorized' does.
module YesodCoreTest.RouteAuthRuntime
    ( specs
    ) where

import Test.Hspec
import Yesod.Core

import YesodCoreTest.RuntimeHarness (assertRequest)

data AuthApp = AuthApp

mkYesodOpts
    (setRouteAuthorization RouteAuthPerResource defaultOpts)
    "AuthApp"
    [parseRoutesNoCheck|
/open    OpenR    GET
/secret  SecretR  GET POST
/sub     SubR:
    /inner  InnerR  GET POST
|]

instance Yesod AuthApp where
    messageLoggerSource = mempty
    -- Deliberately permissive: if authorization ran through here instead of the
    -- dispatch-supplied authorizers, every request below would be authorized
    -- and the 403 expectations would fail.
    isAuthorized _ _ = pure Authorized

getOpenR :: HandlerFor AuthApp String
getOpenR = pure "OpenR"

getSecretR :: HandlerFor AuthApp String
getSecretR = pure "SecretR"

postSecretR :: HandlerFor AuthApp String
postSecretR = pure "SecretR-post"

getInnerR :: HandlerFor AuthApp String
getInnerR = pure "InnerR"

postInnerR :: HandlerFor AuthApp String
postInnerR = pure "InnerR-post"

-- Authorizers demanded by dispatch. Reads are allowed; writes are denied, so a
-- write yields 'permissionDenied' (403).
authorizeOpenR :: RouteAuthorizer AuthApp
authorizeOpenR = RouteAuthorizer $ \_isWrite -> pure Authorized

authorizeSecretR :: RouteAuthorizer AuthApp
authorizeSecretR = RouteAuthorizer $ \isWrite ->
    pure $ if isWrite then Unauthorized "no writes to secret" else Authorized

authorizeInnerR :: RouteAuthorizer AuthApp
authorizeInnerR = RouteAuthorizer $ \isWrite ->
    pure $ if isWrite then Unauthorized "no writes to inner" else Authorized

app :: IO Application
app = toWaiApp AuthApp

specs :: Spec
specs = describe "dispatch-supplied route authorization (RouteAuthPerResource)" $ do
    it "allows an authorized GET on a top-level leaf" $
        assertRequest app "GET" 200 ["open"] (Just "OpenR")

    it "allows an authorized read on a top-level leaf" $
        assertRequest app "GET" 200 ["secret"] (Just "SecretR")

    it "denies a write on a top-level leaf with 403" $
        assertRequest app "POST" 403 ["secret"] Nothing

    it "runs authorization before a 405: a write to a read-only method is 403, not 405" $
        -- DELETE is not among SecretR's methods (GET POST). Without auth this
        -- is a 405; the authorizer sees isWrite=True and denies first.
        assertRequest app "DELETE" 403 ["secret"] Nothing

    it "allows an authorized read on a nested leaf (genNestedDispatchClauses)" $
        assertRequest app "GET" 200 ["sub", "inner"] (Just "InnerR")

    it "denies a write on a nested leaf with 403" $
        assertRequest app "POST" 403 ["sub", "inner"] Nothing

    it "404s an unmatched path without consulting authorization" $
        assertRequest app "GET" 404 ["nope"] Nothing
