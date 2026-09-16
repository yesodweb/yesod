{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_, unless)
import Data.IORef
import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import qualified Network.Wai.Test as WT
import YesodCoreTest.RouteLeaf.Foundation
import YesodCoreTest.RouteLeaf.Account

-- A separate executable: no root dispatcher or root/sibling policies are
-- linked, and the production leaf instance is used unchanged.
main :: IO ()
main = forM_ [("GET", "7", 200, "handler"), ("GET", "8", 403, "error"), ("POST", "7", 405, "error")] $
    \(method, item, status, end) -> do
        events <- newIORef []
        app <- accountApp (LeafApp events accountMiddleware)
        response <- WT.runSession (WT.request WT.defaultRequest
            { W.requestMethod = method
            , W.pathInfo = ["org", "42", "delegation", "account", "alice", "item", item]
            }) app
        unless (H.statusCode (WT.simpleStatus response) == status) $ fail "isolated response status"
        actual <- readIORef events
        unless (actual == ["middleware", "write classification", "account auth", end]) $
            fail $ "isolated authorization order: " ++ show actual
