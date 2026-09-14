{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.RouteAuthSubtree.Inline (specs) where

import Data.Text (Text)
import Control.Monad (forM_)
import qualified Network.HTTP.Types as H
import Network.Wai (responseLBS)
import Test.Hspec
import Yesod.Core
import YesodCoreTest.RuntimeHarness (assertRequest)

-- A parameterized foundation without setParameterizedSubroute uses inline
-- compatibility dispatch. It must still demand subtree authorizers, with
-- the same captures and fragment values as nested dispatch.
data InlineApp a = InlineApp

mkYesodOpts (setRouteAuthorization RouteAuthSubtree defaultOpts) "InlineApp a" [parseRoutes|
/open OpenR GET
/org/#Int OrgR:
    / OuterR GET
    /account/#Int AccountR:
        /item/#Int ItemR GET POST
        /files/*Texts FilesR GET
        /mount/#Int MountR WaiSubsiteWithAuth getSub
|]

instance Yesod (InlineApp a) where
    messageLoggerSource = mempty
    makeSessionBackend _ = pure Nothing

getOpenR :: HandlerFor (InlineApp a) Text
getOpenR = pure "open"

getOuterR :: Int -> HandlerFor (InlineApp a) Text
getOuterR _ = pure "outer"

getItemR, postItemR :: Int -> Int -> Int -> HandlerFor (InlineApp a) Text
getItemR _ _ _ = pure "item"
postItemR _ _ _ = pure "posted"

getFilesR :: Int -> Int -> [Text] -> HandlerFor (InlineApp a) Text
getFilesR _ _ _ = pure "files"

getSub :: InlineApp a -> Int -> Int -> Int -> WaiSubsiteWithAuth
getSub _ _ _ _ = WaiSubsiteWithAuth $ \_ replyToRequest ->
    replyToRequest $ responseLBS H.status200 [] "subsite"

authorizeOpenR :: RouteAuthorizer (InlineApp a)
authorizeOpenR = RouteAuthorizer $ \_ -> pure Authorized

authorizeOrgR :: Int -> OrgR -> RouteAuthorizer (InlineApp a)
authorizeOrgR org _ = RouteAuthorizer $ \_ ->
    pure $ if org == 1 then Authorized else Unauthorized "wrong org"

authorizeAccountR :: Int -> Int -> AccountR -> RouteAuthorizer (InlineApp a)
authorizeAccountR org account fragment = RouteAuthorizer $ \isWrite ->
    pure $ case fragment of
        ItemR 3 | org == 1 && account == 2 && not isWrite -> Authorized
        FilesR ["one", "two"] | org == 1 && account == 2 -> Authorized
        _ -> Unauthorized "wrong fragment"

-- Mounts have their own parent-site policy, including in subtree mode.
authorizeMountR :: Int -> Int -> Int -> RouteAuthorizer (InlineApp a)
authorizeMountR org account mount = RouteAuthorizer $ \_ ->
    pure $ if (org, account, mount) == (1, 2, 3) then Authorized else Unauthorized "wrong mount"

specs :: Spec
specs = describe "subtree authorization in inline compatibility dispatch" $ do
    let request = assertRequest (toWaiApp (InlineApp :: InlineApp ()))
    it "authorizes top-level leaves and single-capture subtrees" $ do
        request "GET" 200 ["open"] (Just "open")
        request "GET" 200 ["org", "1"] (Just "outer")
        request "GET" 403 ["org", "9"] Nothing
    it "passes every parent capture and the leaf fragment to the subtree authorizer" $ do
        request "GET" 200 ["org", "1", "account", "2", "item", "3"] (Just "item")
        forM_
            [ ["org", "9", "account", "2", "item", "3"]
            , ["org", "1", "account", "9", "item", "3"]
            , ["org", "1", "account", "2", "item", "9"] ] $ \path ->
                request "GET" 403 path Nothing
    it "authorizes writes and 405s through the subtree binding" $
        forM_ ["POST", "DELETE"] $ \method ->
            request method 403 ["org", "1", "account", "2", "item", "3"] Nothing
    it "includes trailing pieces in the subtree fragment" $ do
        request "GET" 200 ["org", "1", "account", "2", "files", "one", "two"] (Just "files")
        request "GET" 403 ["org", "1", "account", "2", "files", "private"] Nothing
    it "uses a separate authorizer for a subsite mount" $ do
        request "GET" 200 ["org", "1", "account", "2", "mount", "3"] (Just "subsite")
        request "GET" 403 ["org", "1", "account", "2", "mount", "9"] Nothing
