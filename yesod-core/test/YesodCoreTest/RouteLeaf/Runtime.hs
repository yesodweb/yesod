{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -Werror=incomplete-patterns #-}

module YesodCoreTest.RouteLeaf.Runtime (specs) where

import Control.Monad (forM_)
import Data.IORef
import Data.Text (Text)
import qualified Network.Wai as W
import qualified Network.Wai.Test as WT
import Test.Hspec
import Yesod.Core hiding (isAuthorized)
import Yesod.Core.RouteLeaf
import YesodCoreTest.RouteLeaf.Foundation
import YesodCoreTest.RouteLeaf.Options
import YesodCoreTest.RouteLeaf.Policy
import YesodCoreTest.RouteLeaf.Account
import YesodCoreTest.RuntimeHarness (assertRequestRaw)

instance AuthorizeRoute (Route LeafApp) where
    isAuthorized () endpoint = do
        record "root auth"
        pure $ case endpoint of
            LeafOpenR -> Authorized
            LeafDeniedR -> Unauthorized "root denied"
            LeafAnyR -> Authorized
            LeafMountR capture PageR
                | capture == 42 -> Authorized
                | otherwise -> Unauthorized "mount denied"

-- OrgR mixes local endpoints and delegation; this pattern is exhaustive.
instance AuthorizeRoute OrgR where
    isAuthorized org LeafOrgHomeR = do
        record "org auth"
        pure $ if org == 42 then Authorized else Unauthorized "org denied"

-- There is deliberately no AuthorizeRoute DelegationR instance.
instance AuthorizeRoute StaticR where
    isAuthorized () LeafStaticHomeR = record "static auth" >> pure Authorized

instance AuthorizeRoute OtherR where
    isAuthorized () LeafOtherHomeR = record "other auth" >> pure (Unauthorized "other denied")

instance YesodSubDispatch LeafSub LeafApp where
    yesodSubDispatch = $(mkYesodSubDispatch [parseRoutes| /page PageR GET |])

getPageR :: SubHandlerFor LeafSub LeafApp Text
getPageR = liftHandler $ record "handler" >> pure "page"

getOpenR, getDeniedR, handleAnyR, getStaticHomeR, getOtherHomeR :: HandlerFor LeafApp Text
getOpenR = record "handler" >> pure "open"
getDeniedR = record "handler" >> pure "denied"
handleAnyR = record "handler" >> pure "any"
getStaticHomeR = record "handler" >> pure "static"
getOtherHomeR = record "handler" >> pure "other"

getOrgHomeR :: Int -> HandlerFor LeafApp Text
getOrgHomeR _ = record "handler" >> pure "home"

mkYesodDispatchOpts leafOpts "LeafApp" leafResources

specs :: Spec
specs = describe "leaf dictionary middleware" $ do
    let check method path status events = do
            ref <- newIORef []
            assertRequestRaw (toWaiAppPlain (LeafApp ref authorizationMiddleware))
                WT.defaultRequest { W.requestMethod = method, W.pathInfo = path }
                status Nothing
            readIORef ref `shouldReturn` events
        trace policy end = ["middleware", "write classification", policy, end]
        account org user leaf = ["org", org, "delegation", "account", user] ++ leaf

    it "runs only the selected policy in ordinary middleware" $
        check "GET" ["open"] 200 (trace "root auth" "handler")

    it "denies without invoking the handler" $
        check "GET" ["denied"] 403 (trace "root auth" "error")

    it "authorizes unrestricted-method handlers" $
        check "POST" ["any"] 200 (trace "root auth" "handler")

    it "gives a mixed fragment only its local endpoint" $ do
        check "GET" ["org", "42", "home"] 200 (trace "org auth" "handler")
        check "GET" ["org", "41", "home"] 403 (trace "org auth" "error")

    it "skips ancestor policies and purely delegating fragments" $
        check "GET" (account "42" "alice" ["item", "7"]) 200 (trace "account auth" "handler")

    it "makes the leaf policy enforce all ancestor captures" $
        forM_ [("41", "alice", "7"), ("42", "bob", "7"), ("42", "alice", "8")] $
            \(org, user, item) -> check "GET" (account org user ["item", item]) 403 (trace "account auth" "error")

    it "authorizes before matched-path 405s" $ do
        check "POST" (account "42" "alice" ["item", "7"]) 405 (trace "account auth" "error")
        check "POST" (account "41" "alice" ["item", "7"]) 403 (trace "account auth" "error")

    it "preserves multipieces and unit parent arguments" $ do
        check "GET" (account "42" "alice" ["files", "one", "two"]) 200 (trace "account auth" "handler")
        check "GET" ["static"] 200 (trace "static auth" "handler")

    it "does not select a sibling policy" $
        check "GET" ["other"] 403 (trace "other auth" "error")

    it "leaves unmatched routes to the explicit 404 policy" $
        check "GET" (account "42" "alice" ["missing"]) 404 ["middleware", "error"]

    it "does not repeat authorization for handler errors" $
        check "GET" (account "42" "alice" ["error"]) 400 (trace "account auth" "handler" ++ ["error"])

    it "projects mounted routes while preserving the child route" $ do
        check "GET" ["mount", "42", "page"] 200 (trace "root auth" "handler")
        check "GET" ["mount", "41", "page"] 403 (trace "root auth" "error")

    it "documents that a subsite miss supplies no parent route" $
        check "GET" ["mount", "41", "missing"] 404 ["middleware", "error"]

    it "retains default middleware response headers" $ do
        ref <- newIORef []
        app <- toWaiAppPlain (LeafApp ref authorizationMiddleware)
        forM_ [["open"], ["denied"]] $ \path -> do
            response <- WT.runSession (WT.request WT.defaultRequest { W.pathInfo = path }) app
            lookup "X-XSS-Protection" (WT.simpleHeaders response) `shouldBe` Just "1; mode=block"

    it "uses the same leaf policy in an isolated nested application" $
        forM_ [("GET", "7", 200, "handler"), ("GET", "8", 403, "error"), ("POST", "7", 405, "error")] $
            \(method, item, status, end) -> do
                ref <- newIORef []
                assertRequestRaw (accountApp (LeafApp ref accountMiddleware)) WT.defaultRequest
                    { W.pathInfo = account "42" "alice" ["item", item], W.requestMethod = method }
                    status Nothing
                readIORef ref `shouldReturn` trace "account auth" end

    it "fetches an existential and recovers the request-selected fragment's dictionary" $
        forM_
            [ (["open"], 200, Just "OpenR")
            , (["org", "42", "home"], 200, Just "OrgHomeR")
            , (account "42" "alice" ["item", "7"], 200, Just "ItemR 7")
            , (["static"], 200, Just "StaticHomeR")
            , (["missing"], 404, Nothing)
            ] $ \(path, status, expectedName) -> do
                ref <- newIORef []
                observed <- newIORef Nothing
                let middleware :: HandlerFor LeafApp a -> HandlerFor LeafApp a
                    middleware handler = defaultYesodMiddleware $ do
                        selected <- getDeepestLeaves
                        name <- withRouteLeaves @Show (show . fromRouteLeaves)
                        let describe :: SomeRouteLeaf LeafApp -> (([Text], [(Text, Text)]), String)
                            describe leaves = withSomeRouteLeaf @Show leaves $ \args leaf ->
                                (renderRouteNested args $ fromRouteLeaves leaf, show $ fromRouteLeaves leaf)
                        liftIO $ writeIORef observed $ Just (fmap describe selected, name)
                        handler
                assertRequestRaw (toWaiAppPlain (LeafApp ref middleware)) WT.defaultRequest
                    { W.pathInfo = path } status Nothing
                readIORef observed `shouldReturn`
                    Just (fmap (\name -> ((path, []), name)) expectedName, expectedName)

    it "returns callback actions as values for the middleware to execute once" $ do
        ref <- newIORef []
        let middleware :: HandlerFor LeafApp a -> HandlerFor LeafApp a
            middleware handler = defaultYesodMiddleware $ do
                check <- withRouteLeaves @Show $ \leaf ->
                    record (show $ fromRouteLeaves leaf)
                record "selected"
                forM_ check id
                handler
        assertRequestRaw (toWaiAppPlain (LeafApp ref middleware))
            WT.defaultRequest { W.pathInfo = account "42" "alice" ["item", "7"] }
            200 Nothing
        readIORef ref `shouldReturn`
            ["middleware", "write classification", "selected", "ItemR 7", "handler"]

    it "rejects unexpected matched fragments in focused middleware" $ do
        ref <- newIORef []
        assertRequestRaw (toWaiAppPlain (LeafApp ref accountMiddleware))
            WT.defaultRequest { W.pathInfo = ["open"] } 403 Nothing
        readIORef ref `shouldReturn` ["middleware", "write classification", "error"]

    it "lets callers recover another constraint from the same generated instance" $ do
        let matched = OrgR 42 (DelegationR (AccountR "alice" (ItemR 7)))
        withRouteLeaf @Show matched (\args leaf ->
            (renderRouteNested args (fromRouteLeaves leaf), show $ fromRouteLeaves leaf))
            `shouldBe` ((["org", "42", "delegation", "account", "alice", "item", "7"], []), "ItemR 7")

    it "fills delegation branches without evaluating the leaf callback" $ do
        fillInNested (const ("leaf" :: String)) (error "nested evaluated") (OrgHomeR)
            `shouldBe` "leaf"
        fillInNested (error "leaf evaluated") ("nested" :: String) (DelegationR (AccountR "alice" (ItemR 7)))
            `shouldBe` "nested"
        fromRouteLeaves (LeafFilesR ["one", "two"]) `shouldBe` FilesR ["one", "two"]
