{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- This module deliberately imports no route authorizers or dispatch instances.
module YesodCoreTest.RouteAuthHook.Foundation where

import Data.IORef
import Data.Text (Text)
import Yesod.Core hiding (isAuthorized)

data HookApp = HookApp (IORef [String])

mkYesodData "HookApp" [parseRoutes|
/open OpenR GET
/denied DeniedR GET
/any AnyR
/login LoginR GET
/other-route OtherRouteR GET
/mixed MixedR GET POST
/org/#Int OrgR:
    /fallback FallbackR GET
    /account/#Text AccountR:
        /item/#Int ItemR GET POST
        /files/*Texts FilesR GET
        /required RequiredR GET
        /error ErrorR GET
/unrelated UnrelatedR:
    / UnrelatedHomeR GET
|]

-- An application-owned class: the library must not require its instances in
-- the Yesod dictionary, or collapse its argument to Route HookApp.
class RenderRouteNested route => AuthorizeRoute route where
    isAuthorized :: WithParentArgs route -> HandlerFor (ParentSite route) (AuthorizationResult Text)

data AuthorizationResult a = Allowed a | Denied Text | NeedsLogin
    deriving (Eq, Show)

-- Successful authorization returns an application-owned value. The wrapper
-- can discard it, and throws before invoking the handler on failure.
requireAuthorized :: AuthorizeRoute route => WithParentArgs route -> HandlerFor (ParentSite route) Text
requireAuthorized route = do
    result <- isAuthorized route
    case result of
        Allowed value -> pure value
        Denied message -> permissionDenied message
        NeedsLogin -> notAuthenticated

-- A concrete result type works for flat and nested handlers, including 405s.
withAuthorization
    :: AuthorizeRoute route
    => WithParentArgs route
    -> HandlerFor (ParentSite route) TypedContent
    -> HandlerFor (ParentSite route) TypedContent
withAuthorization route handler = requireAuthorized route >> handler

hookRouteOpts :: RouteOpts
hookRouteOpts = setRouteHandlerWrapper
    (\handler route -> [| withAuthorization $route $handler |]) defaultOpts

recordEvent :: String -> HandlerFor HookApp ()
recordEvent event = do
    HookApp events <- getYesod
    liftIO $ modifyIORef' events (++ [event])

instance Yesod HookApp where
    messageLoggerSource = mempty
    makeSessionBackend _ = pure Nothing
    authRoute _ = Just LoginR
    yesodMiddleware handler = do
        recordEvent "before"
        defaultYesodMiddleware $ do
            recordEvent "middleware"
            handler
    errorHandler err = do
        recordEvent "error"
        defaultErrorHandler err
