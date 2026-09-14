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
/org/#Int OrgR:
    /account/#Int AccountR:
        /item/#Int ItemR GET POST
        /mount/#Int MountR WaiSubsiteWithAuth getSub
|]

instance Yesod NestedApp where
    messageLoggerSource = mempty
    makeSessionBackend _ = pure Nothing

getItemR, postItemR :: Int -> Int -> Int -> HandlerFor NestedApp Text
getItemR _ _ _ = pure "item"
postItemR _ _ _ = pure "posted"

getSub :: NestedApp -> Int -> Int -> Int -> WaiSubsiteWithAuth
getSub _ _ _ _ = WaiSubsiteWithAuth $ \_ replyToRequest ->
    replyToRequest $ responseLBS H.status200 [] "subsite"

authorizeAccountR :: Int -> Int -> AccountR -> RouteAuthorizer NestedApp
authorizeAccountR org account fragment = RouteAuthorizer $ \isWrite ->
    pure $ case fragment of
        ItemR 3 | org == 1 && account == 2 && not isWrite -> Authorized
        _ -> Unauthorized "wrong fragment"

authorizeMountR :: Int -> Int -> Int -> RouteAuthorizer NestedApp
authorizeMountR org account mount = RouteAuthorizer $ \_ ->
    pure $ if (org, account, mount) == (1, 2, 3) then Authorized else Unauthorized "wrong mount"

specs :: Spec
specs = describe "subtree authorization in nested dispatch" $ do
    let request = assertRequest (toWaiApp NestedApp)
    it "authorizes captures and methods through the immediate subtree's binding" $ do
        request "GET" 200 ["org", "1", "account", "2", "item", "3"] (Just "item")
        request "GET" 403 ["org", "9", "account", "2", "item", "3"] Nothing
        request "GET" 403 ["org", "1", "account", "2", "item", "9"] Nothing
        forM_ ["POST", "DELETE"] $ \method ->
            request method 403 ["org", "1", "account", "2", "item", "3"] Nothing
    it "authorizes a nested subsite mount with its own captures" $ do
        request "GET" 200 ["org", "1", "account", "2", "mount", "3"] (Just "subsite")
        request "GET" 403 ["org", "1", "account", "9", "mount", "3"] Nothing
