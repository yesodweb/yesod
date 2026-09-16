{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- No policy classes, concrete authorizers, or dispatch instances are imported.
module YesodCoreTest.RouteLeaf.Foundation where

import Data.IORef
import Data.Text (Text)
import Yesod.Core
import YesodCoreTest.RouteLeaf.Options

data LeafSub = LeafSub
mkYesodSubData "LeafSub" [parseRoutes| /page PageR GET |]

data LeafApp = LeafApp
    { leafEvents :: IORef [String]
    , leafMiddleware :: forall a. HandlerFor LeafApp a -> HandlerFor LeafApp a
    }

mkYesodDataOpts leafOpts "LeafApp" leafResources

getLeafSub :: LeafApp -> Int -> LeafSub
getLeafSub _ _ = LeafSub

record :: String -> HandlerFor LeafApp ()
record event = getYesod >>= liftIO . flip modifyIORef' (++ [event]) . leafEvents

instance Yesod LeafApp where
    makeSessionBackend _ = pure Nothing
    messageLoggerSource = mempty
    -- The application-supplied middleware enforces the leaf policy.
    isAuthorized _ _ = pure Authorized
    isWriteRequest _ = record "write classification" >> pure False
    yesodMiddleware handler = do
        record "middleware"
        app <- getYesod
        leafMiddleware app handler
    errorHandler err = record "error" >> defaultErrorHandler err
