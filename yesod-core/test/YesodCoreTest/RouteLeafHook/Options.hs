{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module YesodCoreTest.RouteLeafHook.Options where

import Yesod.Core
import Yesod.Core.RouteLeaf

class HasRouteLeaves route => AuthorizeRoute route where
    authorizeRoute :: ParentArgs route -> RouteLeaves route -> HandlerFor (ParentSite route) ()

hookRouteOpts :: RouteOpts
hookRouteOpts = setRouteLeafHandlerWrapper [t| AuthorizeRoute |]
    (\handler args leaves -> [| authorizeRoute $args $leaves >> $handler |]) defaultOpts
