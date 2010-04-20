{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Yesod.Resource
    ( parseRoutes
    , mkYesod
    , mkYesodSub
    ) where

import Web.Routes.Quasi
import Yesod.Handler
import Language.Haskell.TH.Syntax
import Yesod.Yesod

mkYesod :: String -> [Resource] -> Q [Dec]
mkYesod name res = do
    let name' = mkName name
    let tySyn = TySynInstD ''Routes [ConT $ name'] (ConT $ mkName $ name ++ "Routes")
    let site = mkName $ "site" ++ name
    let gsbod = NormalB $ VarE site
    let yes' = FunD (mkName "getSite") [Clause [] gsbod []]
    let yes = InstanceD [] (ConT ''YesodSite `AppT` ConT name') [yes']
    CreateRoutesResult x y z <- createRoutes $ CreateRoutesSettings
                { crRoutes = mkName $ name ++ "Routes"
                , crApplication = ConT ''YesodApp
                , crArgument = ConT $ mkName name
                , crExplode = VarE $ mkName "runHandler'"
                , crResources = res
                , crSite = site
                }
    return [tySyn, yes, x, y, z]

mkYesodSub :: String -> [Resource] -> Q [Dec]
mkYesodSub name res = do
    let name' = mkName name
    let site = mkName $ "site" ++ name
    let tySyn = TySynInstD ''Routes [ConT name'] (ConT $ mkName $ name ++ "Routes")
    CreateRoutesResult x _ z <- createRoutes $ CreateRoutesSettings
                { crRoutes = mkName $ name ++ "Routes"
                , crApplication = ConT ''YesodApp
                , crArgument = ConT $ mkName name
                , crExplode = VarE $ mkName "runHandlerSub'"
                , crResources = res
                , crSite = site
                }
    return [tySyn, x, z]
