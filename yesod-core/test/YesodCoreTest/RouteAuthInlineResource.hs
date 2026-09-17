{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.RouteAuthInlineResource (specs) where

import Control.Monad (forM_)
import Data.Text (Text)
import Test.Hspec
import Yesod.Core
import YesodCoreTest.RuntimeHarness (assertRequest)

-- A parameterized foundation without parameterized subroutes forces the
-- inline argument spine, independently of nested-dispatch generation.
data App a = App

mkYesodOpts (setRouteAuthorization RouteAuthPerResource defaultOpts) "App a" [parseRoutes|
/org/#Int OrgR:
    /account/#Text AccountR:
        /item/#Int ItemR GET POST
        /files/*Texts FilesR GET
|]

instance Yesod (App a) where
    messageLoggerSource = mempty
    makeSessionBackend _ = pure Nothing

authorizeItemR :: Int -> Text -> Int -> Bool -> HandlerFor (App a) AuthResult
authorizeItemR org account item isWrite =
    pure $ if (org, account, item) == (1, "alice", 2) && not isWrite
        then Authorized else Unauthorized "item denied"

authorizeFilesR :: Int -> Text -> [Text] -> Bool -> HandlerFor (App a) AuthResult
authorizeFilesR org account pieces _ =
    pure $ if (org, account, pieces) == (1, "alice", ["one", "two"])
        then Authorized else Unauthorized "files denied"

getItemR :: Int -> Text -> Int -> HandlerFor (App a) Text
getItemR _ _ _ = pure "item"

postItemR :: Int -> Text -> Int -> HandlerFor (App a) Html
postItemR _ _ _ = pure (toHtml ("posted" :: Text))

getFilesR :: Int -> Text -> [Text] -> HandlerFor (App a) Text
getFilesR _ _ _ = pure "files"

specs :: Spec
specs = describe "per-resource authorization in inline dispatch" $ do
    let request = assertRequest (toWaiApp (App :: App ()))
    it "passes both parent captures and the leaf capture" $
        request "GET" 200 ["org", "1", "account", "alice", "item", "2"] (Just "item")
    forM_
        [ ["org", "9", "account", "alice", "item", "2"]
        , ["org", "1", "account", "bob", "item", "2"]
        , ["org", "1", "account", "alice", "item", "9"] ] $ \path ->
            it ("rejects changed captures: " ++ show path) $
                request "GET" 403 path Nothing
    forM_ ["POST", "DELETE"] $ \method ->
        it ("checks authorization before method dispatch: " ++ show method) $
            request method 403 ["org", "1", "account", "alice", "item", "2"] Nothing
    it "passes parent captures before the trailing multipiece capture" $ do
        request "GET" 200 ["org", "1", "account", "alice", "files", "one", "two"] (Just "files")
        request "GET" 403 ["org", "9", "account", "alice", "files", "one", "two"] Nothing
        request "GET" 403 ["org", "1", "account", "bob", "files", "one", "two"] Nothing
        request "GET" 403 ["org", "1", "account", "alice", "files", "private"] Nothing
