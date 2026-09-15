{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module YesodCoreTest.RouteAuthSplit.Admin where

import Yesod.Core
import YesodCoreTest.RouteAuthSplit.Foundation

mkYesodDispatchOpts (setFocusOnNestedRoute "AdminR" authOpts) "App" resourcesApp

authorizeUserR :: Int -> Int -> RouteAuthorizer App
authorizeUserR admin user = RouteAuthorizer $ \_ -> do
    record "fragment auth"
    pure $ if (admin, user) == (1, 2) then Authorized else Unauthorized "denied"

getUserR :: Int -> Int -> HandlerFor App String
getUserR _ _ = record "fragment handler" >> pure "user"
