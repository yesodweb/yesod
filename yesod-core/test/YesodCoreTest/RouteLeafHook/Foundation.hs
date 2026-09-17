{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- This module deliberately imports no route authorizers or dispatch instances.
module YesodCoreTest.RouteLeafHook.Foundation where

import Data.IORef
import Data.Text (Text)
import Yesod.Core
import YesodCoreTest.RouteLeafHook.Options

data LeafApp = LeafApp (IORef [String])

mkYesodDataOpts hookRouteOpts "LeafApp" [parseRoutes|
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

recordEvent :: String -> HandlerFor LeafApp ()
recordEvent event = do
    LeafApp events <- getYesod
    liftIO $ modifyIORef' events (++ [event])

instance Yesod LeafApp where
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
