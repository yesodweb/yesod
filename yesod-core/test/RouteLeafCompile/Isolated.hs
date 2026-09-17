{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.IORef
import qualified Network.Wai as W
import qualified Network.Wai.Test as WT
import YesodCoreTest.RouteLeafHook.Account (accountApp)
import YesodCoreTest.RouteLeafHook.AccountPolicy ()
import YesodCoreTest.RouteLeafHook.Foundation
import YesodCoreTest.RuntimeHarness (assertRequestRaw)

-- No root, OrgR, or UnrelatedR policy is imported or defined in this program.
main :: IO ()
main = do
    ref <- newIORef []
    let check method org status = assertRequestRaw (accountApp (LeafApp ref))
            WT.defaultRequest
                { W.requestMethod = method
                , W.pathInfo = ["org", org, "account", "alice", "item", "7"]
                } status Nothing
    check "GET" "42" 200
    check "POST" "42" 403
    check "DELETE" "42" 405
