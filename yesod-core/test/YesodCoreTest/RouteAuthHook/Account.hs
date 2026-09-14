{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- Neither the whole-site authorizer nor the unrelated fragment's authorizer
-- is in scope here. Compilation pins the dependency boundary of the hook.
module YesodCoreTest.RouteAuthHook.Account where

import Data.Text (Text)
import Data.Proxy (Proxy(..))
import qualified Network.Wai as W
import Yesod.Core hiding (isAuthorized)
import YesodCoreTest.RouteAuthHook.Foundation

instance AuthorizeRoute AccountR where
    isAuthorized (WithParentArgs (org, account) route) = do
        recordEvent "account"
        method <- W.requestMethod <$> waiRequest
        pure $ case route of
            ItemR item
                | org == 42 && account == "alice" && item == 7 && method /= "POST" -> Allowed "account"
            FilesR ["one", "two"]
                | org == 42 && account == "alice" -> Allowed "files"
            RequiredR -> NeedsLogin
            ErrorR -> Allowed "error"
            _ -> Denied "account access denied"

mkYesodDispatchOpts
    (setFocusOnNestedRoute "AccountR" $
        setRouteHandlerWrapper
            (\handler route -> [| requireAuthorized $route >> $handler |]) defaultOpts)
    "HookApp"
    resourcesHookApp

-- This function also compiles without YesodDispatch HookApp in scope.
accountApp :: HookApp -> IO Application
accountApp = toWaiAppPlainNested (Proxy :: Proxy AccountR) (42, "alice")

getItemR :: Int -> Text -> Int -> HandlerFor HookApp String
getItemR _ _ _ = recordEvent "handler" >> pure "item"

postItemR :: Int -> Text -> Int -> HandlerFor HookApp String
postItemR _ _ _ = recordEvent "handler" >> pure "posted"

getFilesR :: Int -> Text -> [Text] -> HandlerFor HookApp String
getFilesR _ _ _ = recordEvent "handler" >> pure "files"

getRequiredR :: Int -> Text -> HandlerFor HookApp String
getRequiredR _ _ = recordEvent "handler" >> pure "required"

getErrorR :: Int -> Text -> HandlerFor HookApp String
getErrorR _ _ = recordEvent "handler" >> invalidArgs ["bad input"]
