{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module YesodCoreTest.RouteLeafHook.Runtime (specs) where

import Data.IORef
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import qualified Network.Wai.Test as WT
import Test.Hspec
import Yesod.Core
import YesodCoreTest.RouteLeafHook.Options
import YesodCoreTest.RouteLeafHook.AccountPolicy ()
import YesodCoreTest.RouteLeafHook.Foundation
import YesodCoreTest.RouteLeafHook.Account (accountApp)
import YesodCoreTest.RuntimeHarness (assertRequestRaw)

instance AuthorizeRoute (Route LeafApp) where
    authorizeRoute () route = do
        recordEvent "root"
        case route of
            LeafOpenR -> pure ()
            LeafAnyR -> pure ()
            LeafDeniedR -> permissionDenied "root access denied"
            LeafLoginR -> pure ()
            LeafOtherRouteR -> pure ()
            LeafMixedR -> pure ()

instance AuthorizeRoute OrgR where
    authorizeRoute org LeafFallbackR = do
        recordEvent "org"
        if org == 42 then pure () else permissionDenied "wrong org"

instance AuthorizeRoute UnrelatedR where
    authorizeRoute () LeafUnrelatedHomeR = recordEvent "unrelated" >> permissionDenied "unrelated denied"

mkYesodDispatchOpts
    hookRouteOpts
    "LeafApp"
    resourcesLeafApp

getOpenR, getDeniedR, handleAnyR, getLoginR, getOtherRouteR :: HandlerFor LeafApp String
getOpenR = recordEvent "handler" >> pure "open"
getDeniedR = recordEvent "handler" >> pure "denied"
handleAnyR = recordEvent "handler" >> pure "any"
getLoginR = pure "login"
getOtherRouteR = authorizeRoute (42, "alice") (LeafItemR 7) >> pure "allowed"

getFallbackR :: Int -> HandlerFor LeafApp String
getFallbackR _ = recordEvent "handler" >> pure "fallback"

getMixedR :: HandlerFor LeafApp Text
getMixedR = recordEvent "handler" >> pure "mixed-read"

postMixedR :: HandlerFor LeafApp Html
postMixedR = recordEvent "handler" >> pure (toHtml ("mixed-write" :: Text))

getUnrelatedHomeR :: HandlerFor LeafApp String
getUnrelatedHomeR = pure "unrelated"

specs :: Spec
specs = describe "local leaf dispatch hook" $ do
    let check method path status body events = do
            ref <- newIORef []
            assertRequestRaw (toWaiApp (LeafApp ref)) WT.defaultRequest
                { W.requestMethod = method
                , W.pathInfo = path
                } status body
            readIORef ref `shouldReturn` events
        accountPath :: Text -> Text -> Text -> [Text]
        accountPath org account item = ["org", org, "account", account, "item", item]

    it "runs the callback inside middleware and before the handler" $
        check "GET" ["open"] 200 (Just "open") ["before", "middleware", "root", "handler"]

    it "wraps both allowed and denied top-level method mismatches" $ do
        check "POST" ["open"] 405 Nothing ["before", "middleware", "root", "error"]
        check "POST" ["denied"] 403 Nothing ["before", "middleware", "root", "error"]

    it "wraps handlers without a method restriction" $
        check "POST" ["any"] 200 (Just "any") ["before", "middleware", "root", "handler"]

    it "normalizes different method result types for a concrete wrapper" $ do
        check "GET" ["mixed"] 200 (Just "mixed-read") ["before", "middleware", "root", "handler"]
        check "POST" ["mixed"] 200 (Just "mixed-write") ["before", "middleware", "root", "handler"]

    it "passes a single parent capture without tuple wrapping" $ do
        check "GET" ["org", "42", "fallback"] 200 (Just "fallback")
            ["before", "middleware", "org", "handler"]
        check "GET" ["org", "41", "fallback"] 403 Nothing
            ["before", "middleware", "org", "error"]

    it "dispatches through a separately compiled fragment with its own class instance" $
        check "GET" (accountPath "42" "alice" "7") 200 (Just "item")
            ["before", "middleware", "account", "handler"]

    it "can build and run the fragment application without whole-site dispatch" $ do
        ref <- newIORef []
        assertRequestRaw (accountApp (LeafApp ref)) WT.defaultRequest
            { W.pathInfo = accountPath "42" "alice" "7" } 200 (Just "item")
        readIORef ref `shouldReturn` ["before", "middleware", "account", "handler"]

    it "passes every ancestor capture and the leaf capture to authorization" $
        forM_
            [ accountPath "41" "alice" "7"
            , accountPath "42" "bob" "7"
            , accountPath "42" "alice" "8"
            ] $ \path ->
                check "GET" path 403 Nothing ["before", "middleware", "account", "error"]

    it "throws on failed authorization without running the handler" $
        check "POST" (accountPath "42" "alice" "7") 403 Nothing ["before", "middleware", "account", "error"]

    it "checks authorization before exposing a method mismatch" $ do
        check "DELETE" (accountPath "42" "alice" "7") 405 Nothing
            ["before", "middleware", "account", "error"]
        check "DELETE" (accountPath "41" "alice" "7") 403 Nothing
            ["before", "middleware", "account", "error"]

    it "includes trailing multipieces in the fragment" $
        check "GET" ["org", "42", "account", "alice", "files", "one", "two"] 200 (Just "files")
            ["before", "middleware", "account", "handler"]

    it "can call the class method for a different route" $
        check "GET" ["other-route"] 200 (Just "allowed") ["before", "middleware", "root", "account"]

    it "does not authorize an unmatched path" $
        check "GET" ["org", "42", "account", "alice", "missing"] 404 Nothing
            ["before", "middleware", "error"]

    it "does not repeat authorization while rendering handler errors" $
        check "GET" ["org", "42", "account", "alice", "error"] 400 Nothing
            ["before", "middleware", "account", "handler", "error"]

    it "lets the callback choose how to report authentication failures" $ do
        ref <- newIORef []
        app <- toWaiApp (LeafApp ref)
        let req = WT.defaultRequest
                { W.pathInfo = ["org", "42", "account", "alice", "required"] }
        WT.runSession (do
            html <- WT.request req
            WT.assertStatus 401 html
            json <- WT.request req { W.requestHeaders = [(H.hAccept, "application/json")] }
            WT.assertStatus 401 json) app

    it "keeps sibling authorization separate" $
        check "GET" ["unrelated"] 403 Nothing ["before", "middleware", "unrelated", "error"]
