{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans -Werror=incomplete-patterns #-}

-- Compiles without root dispatch, root policies, or sibling policies.
module YesodCoreTest.RouteLeaf.Account where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Yesod.Core hiding (isAuthorized)
import Yesod.Core.RouteLeaf
import YesodCoreTest.RouteLeaf.Foundation
import YesodCoreTest.RouteLeaf.Options
import YesodCoreTest.RouteLeaf.Policy

instance AuthorizeRoute AccountR where
    isAuthorized (org, account) endpoint = do
        record "account auth"
        pure $ if org /= 42 || account /= "alice"
            then Unauthorized "parent captures denied"
            else case endpoint of
                AuthItemR 7 -> Authorized
                AuthItemR _ -> Unauthorized "item denied"
                AuthFilesR ["one", "two"] -> Authorized
                AuthFilesR _ -> Unauthorized "files denied"
                AuthErrorR -> Authorized

mkYesodDispatchOpts (setFocusOnNestedRoute "AccountR" leafOpts) "LeafApp" leafResources

getItemR :: Int -> Text -> Int -> HandlerFor LeafApp Text
getItemR _ _ _ = record "handler" >> pure "item"

getFilesR :: Int -> Text -> [Text] -> HandlerFor LeafApp Text
getFilesR _ _ _ = record "handler" >> pure "files"

getErrorR :: Int -> Text -> HandlerFor LeafApp Text
getErrorR _ _ = record "handler" >> invalidArgs ["bad input"]

-- A total site-wide dictionary would require siblings here. Instead, the
-- focused application checks the generated witness and uses its own policy.
accountMiddleware :: HandlerFor LeafApp a -> HandlerFor LeafApp a
accountMiddleware handler = defaultYesodMiddlewareNoAuthCheck $ do
    current <- getCurrentRoute
    case fmap routeLeaf current of
        Nothing -> handler
        Just (SomeRouteLeaf LeafAccountR args leaf) -> do
            dispatchAuthorizationCheck (const $ isAuthorized args leaf)
            handler
        Just _ -> permissionDenied "unexpected endpoint in focused application"

accountApp :: LeafApp -> IO Application
accountApp = toWaiAppPlainNested (Proxy :: Proxy AccountR) (42, "alice")
