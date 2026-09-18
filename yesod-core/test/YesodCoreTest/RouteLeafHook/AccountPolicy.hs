{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module YesodCoreTest.RouteLeafHook.AccountPolicy () where
import Yesod.Core
import qualified Network.Wai as W
import YesodCoreTest.RouteLeafHook.Foundation
import YesodCoreTest.RouteLeafHook.Options

instance AuthorizeRoute AccountR where
    authorizeRoute (org, account) route = do
        recordEvent "account"
        method <- W.requestMethod <$> waiRequest
        case route of
            ItemR item
                | org == 42 && account == "alice" && item == 7 && method /= "POST" -> pure ()
            FilesR ["one", "two"]
                | org == 42 && account == "alice" -> pure ()
            RequiredR -> notAuthenticated
            ErrorR -> pure ()
            _ -> permissionDenied "account access denied"

