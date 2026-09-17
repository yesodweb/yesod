{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.RouteAuthSubtree.Nested (specs) where

import Data.Text (Text)
import Control.Monad (forM_)
import qualified Network.HTTP.Types as H
import Network.Wai (responseLBS)
import Test.Hspec
import Yesod.Core
import YesodCoreTest.RuntimeHarness (assertRequest)

data NestedApp = NestedApp

mkYesodOpts (setRouteAuthorization RouteAuthSubtree defaultOpts) "NestedApp" [parseRoutes|
/static StaticR:
    /leaf StaticLeafR GET POST
/org/#Int OrgR:
    /account/#Int AccountR:
        /item/#Int ItemR GET POST
        /mount/#Int MountR WaiSubsiteWithAuth getSub
|]

instance Yesod NestedApp where
    messageLoggerSource = mempty
    makeSessionBackend _ = pure Nothing

getStaticLeafR, postStaticLeafR :: HandlerFor NestedApp Text
getStaticLeafR = pure "static"
postStaticLeafR = pure "static-post"

authorizeStaticR :: StaticR -> Bool -> HandlerFor NestedApp AuthResult
authorizeStaticR StaticLeafR isWrite =
    pure $ if isWrite then Unauthorized "static write" else Authorized

getItemR, postItemR :: Int -> Int -> Int -> HandlerFor NestedApp Text
getItemR _ _ _ = pure "item"
postItemR _ _ _ = pure "posted"

getSub :: NestedApp -> Int -> Int -> Int -> WaiSubsiteWithAuth
getSub _ _ _ _ = WaiSubsiteWithAuth $ \_ replyToRequest ->
    replyToRequest $ responseLBS H.status200 [] "subsite"

authorizeAccountR :: Int -> Int -> AccountR -> Bool -> HandlerFor NestedApp AuthResult
authorizeAccountR org account fragment isWrite =
    pure $ case fragment of
        ItemR 3 | org == 1 && account == 2 && not isWrite -> Authorized
        _ -> Unauthorized "wrong fragment"

authorizeMountR :: Int -> Int -> Int -> Bool -> HandlerFor NestedApp AuthResult
authorizeMountR org account mount _ =
    pure $ if (org, account, mount) == (1, 2, 3) then Authorized else Unauthorized "wrong mount"

specs :: Spec
specs = describe "subtree authorization in nested dispatch" $ do
    let request = assertRequest (toWaiApp NestedApp)
    it "passes a fragment with no parent captures to its subtree authorizer" $ do
        request "GET" 200 ["static", "leaf"] (Just "static")
        forM_ ["POST", "DELETE"] $ \method ->
            request method 403 ["static", "leaf"] Nothing
    it "authorizes captures and methods through the immediate subtree's binding" $ do
        request "GET" 200 ["org", "1", "account", "2", "item", "3"] (Just "item")
        request "GET" 403 ["org", "9", "account", "2", "item", "3"] Nothing
        request "GET" 403 ["org", "1", "account", "2", "item", "9"] Nothing
        forM_ ["POST", "DELETE"] $ \method ->
            request method 403 ["org", "1", "account", "2", "item", "3"] Nothing
    it "authorizes a nested subsite mount with its own captures" $ do
        request "GET" 200 ["org", "1", "account", "2", "mount", "3"] (Just "subsite")
        request "GET" 403 ["org", "1", "account", "9", "mount", "3"] Nothing
