{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Route.AuthCleared (app) where

import Yesod.Core

data ClearedApp = ClearedApp

mkYesodOpts
    (unsetRouteHandlerWrapper $
        setRouteHandlerWrapper (\_ _ -> fail "cleared wrapper ran") $
        setRouteAuthorization RouteAuthPerResource defaultOpts)
    "ClearedApp" [parseRoutes| / ClearedR GET |]

instance Yesod ClearedApp where
    messageLoggerSource = mempty
    makeSessionBackend _ = pure Nothing

getClearedR :: HandlerFor ClearedApp String
getClearedR = pure "cleared"

authorizeClearedR :: Bool -> HandlerFor ClearedApp AuthResult
authorizeClearedR isWrite =
    pure $ if isWrite then Unauthorized "no writes" else Authorized

app :: IO Application
app = toWaiApp ClearedApp
