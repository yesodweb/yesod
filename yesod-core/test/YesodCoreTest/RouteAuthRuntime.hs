{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
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
-- Unless explicitly denied by a request header, 'isAuthorized' allows
-- everything. Named denials therefore come from generated dispatch, including
-- before 405s. The foundation can also select middleware without the legacy
-- check, exercising the same dispatch through both middleware variants.
module YesodCoreTest.RouteAuthRuntime
    ( specs
    ) where

import Test.Hspec
import Yesod.Core
import Control.Monad (forM_)
import Data.IORef
import Data.Text (Text)
import qualified Network.Wai as W
import qualified Network.Wai.Test as WT

import YesodCoreTest.RuntimeHarness (assertRequest, assertRequestRaw)

data AuthSub = AuthSub

mkYesodSubData "AuthSub" [parseRoutes|
/page PageR GET
/writable WritableR GET DELETE
|]

data AuthApp = AuthApp (IORef [String]) Bool

mkYesodOpts
    (setRouteHandlerWrapper (\handler _ -> [| wrapper $handler |]) $
        setRouteAuthorization RouteAuthPerResource defaultOpts)
    "AuthApp"
    [parseRoutesNoCheck|
/open    OpenR    GET
/secret  SecretR  GET POST
/mount/#Int MountR AuthSub getAuthSub
/sub/#Int SubR:
    /inner  InnerR  GET POST
    /mount/#Int NestedMountR AuthSub getNestedAuthSub
/static StaticR:
    /leaf StaticLeafR GET POST
|]

instance YesodSubDispatch AuthSub AuthApp where
    -- The locally generated resourcesAuthSub cannot be used in this splice
    -- because of TH's stage restriction.
    yesodSubDispatch = $(mkYesodSubDispatch [parseRoutes|
/page PageR GET
/writable WritableR GET DELETE
|])

getAuthSub :: AuthApp -> Int -> AuthSub
getAuthSub _ _ = AuthSub

getNestedAuthSub :: AuthApp -> Int -> Int -> AuthSub
getNestedAuthSub _ _ _ = AuthSub

record :: String -> HandlerFor AuthApp ()
record event = do
    AuthApp ref _ <- getYesod
    liftIO $ modifyIORef' ref (++ [event])

getPageR :: SubHandlerFor AuthSub AuthApp Text
getPageR = liftHandler $ record "handler" >> pure "subsite"

getWritableR, deleteWritableR :: SubHandlerFor AuthSub AuthApp Text
getWritableR = getPageR
deleteWritableR = getPageR

wrapper :: HandlerFor AuthApp TypedContent -> HandlerFor AuthApp TypedContent
wrapper handler = do
    record "wrapper"
    deny <- lookupHeader "X-Deny-Wrapper"
    case deny of
        Just _ -> notAuthenticated
        Nothing -> handler

instance Yesod AuthApp where
    messageLoggerSource = mempty
    isAuthorized _ _ = do
        record "legacy"
        deny <- lookupHeader "X-Deny-Legacy"
        pure $ if deny == Just "yes" then Unauthorized "legacy denied" else Authorized
    isWriteRequest _ = do
        record "write"
        forceRead <- lookupHeader "X-Treat-As-Read"
        method <- W.requestMethod <$> waiRequest
        pure $ forceRead /= Just "yes" && method `notElem` ["GET", "HEAD", "OPTIONS", "TRACE"]
    makeSessionBackend _ = pure Nothing
    yesodMiddleware handler = do
        record "before"
        AuthApp _ legacyCheck <- getYesod
        result <- if legacyCheck
            then defaultYesodMiddleware handler
            else defaultYesodMiddlewareNoAuthCheck handler
        record "after"
        pure result
    errorHandler err = record "error" >> defaultErrorHandler err

getOpenR :: HandlerFor AuthApp String
getOpenR = record "handler" >> pure "OpenR"

getSecretR :: HandlerFor AuthApp String
getSecretR = record "handler" >> pure "SecretR"

postSecretR :: HandlerFor AuthApp String
postSecretR = record "handler" >> pure "SecretR-post"

getInnerR :: Int -> HandlerFor AuthApp String
getInnerR _ = record "handler" >> pure "InnerR"

postInnerR :: Int -> HandlerFor AuthApp String
postInnerR _ = record "handler" >> pure "InnerR-post"

getStaticLeafR, postStaticLeafR :: HandlerFor AuthApp String
getStaticLeafR = record "handler" >> pure "static"
postStaticLeafR = record "handler" >> pure "static-post"

authorizeStaticLeafR :: RouteAuthorizer AuthApp
authorizeStaticLeafR = authorizeSecretR

-- Authorizers demanded by dispatch. Reads are allowed; writes are denied, so a
-- write yields 'permissionDenied' (403).
authorizeOpenR :: RouteAuthorizer AuthApp
authorizeOpenR = RouteAuthorizer $ \_isWrite -> record "named" >> pure Authorized

authorizeSecretR :: RouteAuthorizer AuthApp
authorizeSecretR = RouteAuthorizer $ \isWrite -> do
    record "named"
    pure $ if isWrite then Unauthorized "no writes to secret" else Authorized

authorizeInnerR :: Int -> RouteAuthorizer AuthApp
authorizeInnerR _ = RouteAuthorizer $ \isWrite -> do
    record "named"
    pure $ if isWrite then Unauthorized "no writes to inner" else Authorized

authorizeMountR :: Int -> RouteAuthorizer AuthApp
authorizeMountR mount = authorizeNestedMountR 1 mount

authorizeNestedMountR :: Int -> Int -> RouteAuthorizer AuthApp
authorizeNestedMountR parent mount = RouteAuthorizer $ \isWrite -> do
    record "named"
    pure $ if parent == 1 && mount == 2 && not isWrite
        then Authorized else Unauthorized "private subsite"

app :: IO Application
app = newIORef [] >>= \ref -> toWaiApp (AuthApp ref True)

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
        assertRequest app "GET" 200 ["sub", "1", "inner"] (Just "InnerR")

    it "denies a write on a nested leaf with 403" $
        assertRequest app "POST" 403 ["sub", "1", "inner"] Nothing

    it "authorizes a leaf under a parent with no dynamic arguments" $ do
        assertRequest app "GET" 200 ["static", "leaf"] (Just "static")
        forM_ ["POST", "DELETE"] $ \method ->
            assertRequest app method 403 ["static", "leaf"] Nothing

    it "404s an unmatched path without consulting authorization" $
        assertRequest app "GET" 404 ["nope"] Nothing

    let checkWithMiddleware legacyCheck method path headers status events = do
            ref <- newIORef []
            assertRequestRaw (toWaiApp (AuthApp ref legacyCheck)) WT.defaultRequest
                { W.requestMethod = method, W.pathInfo = path, W.requestHeaders = headers }
                status Nothing
            readIORef ref `shouldReturn` events
        check = checkWithMiddleware True
        checkDispatchOnly = checkWithMiddleware False

    it "runs legacy auth, named auth, and the wrapper inside middleware in order" $
        forM_ [["secret"], ["sub", "1", "inner"]] $ \path ->
            check "GET" path [] 200 ["before", "write", "legacy", "write", "named", "wrapper", "handler", "after"]

    it "stops at named auth when both checks would deny, including 405s" $
        forM_ [(method, path) | method <- ["POST", "DELETE"], path <- [["secret"], ["sub", "1", "inner"]]] $ \(method, path) ->
            check method path [("X-Deny-Wrapper", "yes")] 403 ["before", "write", "legacy", "write", "named", "error"]

    it "can deny in the wrapper after named auth succeeds" $
        check "GET" ["secret"] [("X-Deny-Wrapper", "yes")] 401
            ["before", "write", "legacy", "write", "named", "wrapper", "error"]

    it "authorizes flat and nested subsite mounts before their handlers" $
        forM_ [["mount", "2", "page"], ["sub", "1", "mount", "2", "page"]] $ \path ->
            check "GET" path [] 200 ["before", "write", "legacy", "write", "named", "handler", "after"]

    it "denies subsite mounts using ancestor and mount captures" $
        forM_ [["mount", "3", "page"], ["sub", "9", "mount", "2", "page"], ["sub", "1", "mount", "3", "page"]] $ \path ->
            check "GET" path [] 403 ["before", "write", "legacy", "write", "named", "error"]

    it "authorizes subsite method mismatches and unmatched subsite paths" $ do
        check "DELETE" ["mount", "2", "page"] [] 403 ["before", "write", "legacy", "write", "named", "error"]
        check "GET" ["mount", "3", "missing"] [] 403 ["before", "named", "error"]
        check "GET" ["mount", "2", "missing"] [] 404 ["before", "named", "error"]
        check "DELETE" ["mount", "2", "missing"] [] 403 ["before", "named", "error"]

    it "uses a site's read override on mount hits and the default on misses" $
        forM_ [["mount", "2"], ["sub", "1", "mount", "2"]] $ \prefix -> do
            check "DELETE" (prefix ++ ["writable"]) [("X-Treat-As-Read", "yes")] 200
                ["before", "write", "legacy", "write", "named", "handler", "after"]
            check "DELETE" (prefix ++ ["missing"]) [("X-Treat-As-Read", "yes")] 403
                ["before", "named", "error"]

    it "retains legacy denials in defaultYesodMiddleware" $
        forM_ [["secret"], ["sub", "1", "inner"]] $ \path ->
            check "GET" path [("X-Deny-Legacy", "yes")] 403
                ["before", "write", "legacy", "error"]

    it "skips the legacy policy and its method classification with NoAuthCheck" $
        forM_ [["secret"], ["sub", "1", "inner"]] $ \path ->
            checkDispatchOnly "GET" path [("X-Deny-Legacy", "yes")] 200
                ["before", "write", "named", "wrapper", "handler", "after"]

    it "retains named denials before handlers and 405s with NoAuthCheck" $
        forM_ [(method, path) | method <- ["POST", "DELETE"], path <- [["secret"], ["sub", "1", "inner"]]] $ \(method, path) ->
            checkDispatchOnly method path [] 403 ["before", "write", "named", "error"]

    it "retains wrapper denials with NoAuthCheck" $
        forM_ [["secret"], ["sub", "1", "inner"]] $ \path ->
            checkDispatchOnly "GET" path [("X-Deny-Wrapper", "yes")] 401
                ["before", "write", "named", "wrapper", "error"]

    it "still applies the site's method override once for named checks with NoAuthCheck" $
        forM_ [["secret"], ["sub", "1", "inner"]] $ \path ->
            checkDispatchOnly "POST" path [("X-Treat-As-Read", "yes")] 200
                ["before", "write", "named", "wrapper", "handler", "after"]

    it "retains named mount authorization on hits and misses with NoAuthCheck" $
        forM_ [["mount", "2"], ["sub", "1", "mount", "2"]] $ \prefix -> do
            checkDispatchOnly "GET" (prefix ++ ["page"]) [] 200
                ["before", "write", "named", "handler", "after"]
            checkDispatchOnly "DELETE" (prefix ++ ["page"]) [] 403
                ["before", "write", "named", "error"]
            checkDispatchOnly "GET" (prefix ++ ["missing"]) [] 404
                ["before", "named", "error"]
            checkDispatchOnly "DELETE" (prefix ++ ["missing"]) [] 403
                ["before", "named", "error"]

    it "preserves the default headers on allowed and denied responses with either middleware" $
        forM_ [True, False] $ \legacyCheck -> do
            ref <- newIORef []
            application <- toWaiAppPlain (AuthApp ref legacyCheck)
            WT.runSession (forM_ [("GET", 200), ("POST", 403)] $ \(method, status) -> do
                response <- WT.request WT.defaultRequest
                    { W.requestMethod = method, W.pathInfo = ["secret"] }
                WT.assertStatus status response
                WT.assertHeader "Vary" "Accept, Accept-Language" response
                WT.assertHeader "X-XSS-Protection" "1; mode=block" response) application
