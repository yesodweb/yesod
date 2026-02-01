{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.NestedDispatch.Parent0R.Child0R where

import Data.Text (Text)
import qualified Data.Text as Text
import Yesod.Core.Handler
import YesodCoreTest.NestedDispatch.Resources
import Yesod.Core
import qualified Network.Wai as W

mkYesodDataOpts (setFocusOnNestedRoute (Just "Child0R") defaultOpts) "App" nestedDispatchResources

instance YesodDispatchNested Child0R where
    yesodDispatchNested _ (i, txt) mkParentRoute yre req =
        case drop 4 (W.pathInfo req) of
            [] ->
                Just $
                    let route =
                            mkParentRoute ParentChildIndexR
                        handler =
                            case W.requestMethod req of
                                "GET" ->
                                    fmap toTypedContent (getParentChildIndexR i txt)
                                "POST" ->
                                    fmap toTypedContent (postParentChildIndexR i txt)
                                _ ->
                                    fmap (toTypedContent :: () -> TypedContent) badMethod
                    in
                        yesodRunner handler yre (Just route) req

            [fromPathPiece -> Just str] ->
                Just $ \send ->
                    let route =
                            mkParentRoute (ParentChildR str)
                        handler =
                            case W.requestMethod req of
                                "GET" ->
                                    fmap toTypedContent (getParentChildR i txt str)
                                "POST" ->
                                    fmap toTypedContent (postParentChildR i txt str)
                                _ ->
                                    fmap (toTypedContent :: () -> TypedContent) (badMethod)
                    in
                        yesodRunner handler yre (Just route) req send
            _ ->
                Nothing

postParentChildIndexR :: Int -> Text -> HandlerFor App Text
postParentChildIndexR i t = pure $ Text.pack $ show (i, t)

getParentChildIndexR :: Int -> Text -> HandlerFor App Text
getParentChildIndexR = postParentChildIndexR

postParentChildR :: Int -> Text -> String -> HandlerFor App Text
postParentChildR i t c = pure $ Text.pack $ show (i, t, c)

getParentChildR :: Int -> Text -> String -> HandlerFor App Text
getParentChildR = postParentChildR
