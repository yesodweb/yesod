{-# LANGUAGE QuasiQuotes #-}
module YesodCoreTest.RouteLeaf.SplitResources where

import Yesod.Core
import Yesod.Routes.TH.Types (ResourceTree)

data SplitLeafApp a = SplitLeafApp

splitLeafOpts :: RouteOpts
splitLeafOpts = setRouteLeafViews True $ setParameterizedSubroute True defaultOpts

splitLeafResources :: [ResourceTree String]
splitLeafResources = [parseRoutes|
/top TopR GET
/scope/#Int ScopeR:
    /local/#Text LocalR GET
    /group GroupR:
        /child/#Text ChildR:
            /item/#Int SplitItemR GET
|]
