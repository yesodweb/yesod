{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-orphans -Werror #-}

-- Named policies in both splices are valid even with warnings as errors.
module YesodCoreTest.RouteAuthSplit.Runtime (specs) where

import Control.Monad (forM_)
import Data.IORef
import Test.Hspec
import Yesod.Core
import YesodCoreTest.RouteAuthSplit.Admin ()
import YesodCoreTest.RouteAuthSplit.Foundation
import YesodCoreTest.RuntimeHarness (assertRequest)

mkYesodDispatchOpts authOpts "App" resourcesApp

authorizeHomeR :: RouteAuthorizer App
authorizeHomeR = RouteAuthorizer $ \_ -> record "root auth" >> pure Authorized

getHomeR :: HandlerFor App String
getHomeR = record "root handler" >> pure "home"

specs :: Spec
specs = describe "named authorization in separately compiled dispatch" $ do
    let check method path status body expected = do
            events <- newIORef []
            assertRequest (toWaiAppPlain (App events)) method status path body
            readIORef events `shouldReturn` expected
    it "runs the root policy for a root handler" $
        check "GET" [] 200 (Just "home") ["root auth", "root handler"]
    it "runs the fragment's policy once before its handler" $
        check "GET" ["admin", "1", "user", "2"] 200 (Just "user")
            ["fragment auth", "fragment handler"]
    forM_ [["admin", "3", "user", "2"], ["admin", "1", "user", "3"]] $ \path ->
        it ("denies changed captures: " ++ show path) $
            check "GET" path 403 Nothing ["fragment auth"]
    it "denies an unauthorized method mismatch before returning 405" $
        check "DELETE" ["admin", "1", "user", "3"] 403 Nothing ["fragment auth"]
    it "authorizes an unsupported method before returning 405" $
        check "DELETE" ["admin", "1", "user", "2"] 405 Nothing ["fragment auth"]
