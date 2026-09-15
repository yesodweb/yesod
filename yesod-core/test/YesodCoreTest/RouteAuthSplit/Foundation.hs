{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module YesodCoreTest.RouteAuthSplit.Foundation where

import Data.IORef
import Yesod.Core

data App = App (IORef [String])

mkYesodData "App" [parseRoutes|
/ HomeR GET
/admin/#Int AdminR:
    /user/#Int UserR GET
|]

authOpts :: RouteOpts
authOpts = setRouteAuthorization RouteAuthPerResource defaultOpts

record :: String -> HandlerFor App ()
record event = do
    App events <- getYesod
    liftIO $ modifyIORef' events (++ [event])

-- No authorizer imports are needed in the foundation.
instance Yesod App where
    messageLoggerSource = mempty
    makeSessionBackend _ = pure Nothing
