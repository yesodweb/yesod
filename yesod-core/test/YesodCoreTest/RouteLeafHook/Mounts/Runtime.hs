{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module YesodCoreTest.RouteLeafHook.Mounts.Runtime (specs) where

import Control.Monad (forM_)
import Data.IORef
import Data.Proxy
import qualified Network.Wai as W
import qualified Network.Wai.Test as WT
import Test.Hspec
import Yesod.Core
import Yesod.Core.Class.Dispatch.ToParentRoute
import Yesod.Core.RouteLeaf
import YesodCoreTest.RouteLeafHook.Mounts.Data
import YesodCoreTest.RouteLeafHook.Mounts.Dispatch ()
import YesodCoreTest.RuntimeHarness (assertRequestRaw)

specs :: Spec
specs = describe "subsite mounts as local leaves" $ do
    let check makeApp parent method path status body events = do
            ref <- newIORef []
            assertRequestRaw (makeApp parent (MountApp ref :: MountApp ())) WT.defaultRequest
                { W.requestMethod = method, W.pathInfo = path } status body
            readIORef ref `shouldReturn` events
        root = const toWaiApp
        fragment = toWaiAppPlainNested (Proxy @(MountGroupR ()))

    forM_ [("root", root), ("fragment", fragment)] $ \(label, makeApp) ->
        describe label $ do
            it "passes parent and mount captures with the selected child" $
                check makeApp 42 "GET" ["group", "42", "mount", "allowed", "page"] 200
                    (Just "authorized") ["middleware", "mount-page", "handler"]
            it "preserves captures on a miss and runs the wrapper before the 404" $
                check makeApp 42 "GET" ["group", "42", "mount", "allowed", "missing"] 404
                    Nothing ["middleware", "mount-miss", "error"]
            it "denies matched routes, method mismatches, and misses before their handlers" $
                forM_ [("GET", "page"), ("POST", "page"), ("GET", "missing")] $ \(method, suffix) ->
                    check makeApp 41 method ["group", "41", "mount", "allowed", suffix] 403
                        Nothing ["middleware", if suffix == "missing" then "mount-miss" else "mount-page", "error"]
            it "checks local mount captures on misses" $
                check makeApp 42 "GET" ["group", "42", "mount", "denied", "missing"] 403
                    Nothing ["middleware", "mount-miss", "error"]
            it "runs once for a matched-path 405" $
                check makeApp 42 "POST" ["group", "42", "mount", "allowed", "page"] 405
                    Nothing ["middleware", "mount-page", "error"]
            it "does not repeat the wrapper while rendering a subsite error" $
                check makeApp 42 "GET" ["group", "42", "mount", "allowed", "error"] 400
                    Nothing ["middleware", "mount-error", "error"]
            it "does not invent a mount leaf outside the mount prefix" $
                check makeApp 42 "GET" ["group", "42", "missing"] 404
                    Nothing ["middleware", "error"]

    it "wraps top-level mounts through the same callback" $ do
        check root 42 "GET" ["mount", "7", "page"] 200 (Just "authorized")
            ["middleware", "mount-page", "handler"]
        check root 42 "GET" ["mount", "8", "missing"] 403 Nothing
            ["middleware", "mount-miss", "error"]

    it "keeps ordinary endpoints on the same callback" $
        check root 42 "GET" ["plain"] 200 (Just "plain") ["middleware", "plain-policy", "handler"]

    it "reports an inconsistent route from a custom subsite as an internal error" $
        check root 42 "GET" ["wrong"] 500 Nothing ["middleware", "error"]

    it "reconstructs matched mounts but does not fabricate routes for misses" $ do
        fromRouteLeaves (LeafNestedMountR "allowed" (Just SubPageR) :: RouteLeaves (MountGroupR ()))
            `shouldBe` Just (NestedMountR "allowed" SubPageR)
        fromRouteLeaves (LeafNestedMountR "allowed" Nothing :: RouteLeaves (MountGroupR ()))
            `shouldBe` Nothing

    it "recovers the original parent captures and rejects routes from another fragment" $ do
        let recover = fromParentRoute @(MountGroupR ())
        case recover (MountGroupR 42 $ NestedMountR "allowed" SubPageR) of
            Just (WithParentArgs parent route) -> do
                parent `shouldBe` 42
                route `shouldBe` NestedMountR "allowed" SubPageR
            Nothing -> expectationFailure "expected the mounted fragment"
        case recover PlainR of
            Nothing -> pure ()
            Just _ -> expectationFailure "accepted a different fragment"
