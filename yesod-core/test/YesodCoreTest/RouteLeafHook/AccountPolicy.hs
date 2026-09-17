{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module YesodCoreTest.RouteLeafHook.AccountPolicy () where
import Yesod.Core
import Yesod.Core.RouteLeaf
import qualified Network.Wai as W
import YesodCoreTest.RouteLeafHook.Foundation
import YesodCoreTest.RouteLeafHook.Options

instance AuthorizeRoute AccountR where
    authorizeRoute (org, account) route = do
        recordEvent "account"
        method <- W.requestMethod <$> waiRequest
        case route of
            LeafItemR item
                | org == 42 && account == "alice" && item == 7 && method /= "POST" -> pure ()
            LeafFilesR ["one", "two"]
                | org == 42 && account == "alice" -> pure ()
            LeafRequiredR -> notAuthenticated
            LeafErrorR -> pure ()
            _ -> permissionDenied "account access denied"

