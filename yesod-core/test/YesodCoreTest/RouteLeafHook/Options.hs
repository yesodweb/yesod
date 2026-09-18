{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module YesodCoreTest.RouteLeafHook.Options where

import Yesod.Core

class AuthorizeRoute route where
    authorizeRoute :: ParentArgs route -> route -> HandlerFor (ParentSite route) ()

hookRouteOpts :: RouteOpts
hookRouteOpts = setRouteDispatchWrapper [t| AuthorizeRoute |]
    (\handler route -> [| let WithParentArgs args fragment = $route in authorizeRoute args fragment >> $handler |]) defaultOpts
