{-# LANGUAGE QuasiQuotes #-}
module YesodCoreTest.RouteLeaf.Options where

import Yesod.Core
import Yesod.Routes.TH.Types (ResourceTree)

leafOpts :: RouteOpts
leafOpts = setRouteLeafViews True defaultOpts

leafResources :: [ResourceTree String]
leafResources = [parseRoutes|
/open OpenR GET
/denied DeniedR GET
/any AnyR
/mount/#Int MountR LeafSub getLeafSub
/org/#Int OrgR:
    /home OrgHomeR GET
    /delegation DelegationR:
        /account/#Text AccountR:
            /item/#Int ItemR GET
            /files/*Texts FilesR GET
            /error ErrorR GET
/static StaticR:
    / StaticHomeR GET
/other OtherR:
    / OtherHomeR GET
|]
