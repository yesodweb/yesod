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
    return [tySyn, yes, x, {-y, -}z]

mkYesodSub :: String -> [Name] -> [Resource] -> Q [Dec]
mkYesodSub name ctxs res = do
    let name' = mkName name
    let site = mkName $ "site" ++ name
    let tySyn = TySynInstD ''Routes [ConT name'] (ConT $ mkName $ name ++ "Routes")
    let sa = ConT (mkName name)
    let man = mkName "master"
    let ma = VarT man -- FIXME
    let sr = ConT $ mkName $ name ++ "Routes"
    let mr = ConT ''Routes `AppT` VarT man
    let arg = TupleT 4
                `AppT` ma
                `AppT` (ArrowT `AppT` ma `AppT` sa)
                `AppT` (ArrowT `AppT` sr `AppT` mr)
                `AppT` (ArrowT `AppT` mr `AppT` ConT ''String)
    CreateRoutesResult x (SigD yname y) z <- createRoutes $ CreateRoutesSettings
                { crRoutes = mkName $ name ++ "Routes"
                , crApplication = ConT ''YesodApp
                , crArgument = arg
                , crExplode = VarE $ mkName "runHandlerSub'"
                , crResources = res
                , crSite = site
                }
    let helper claz = ClassP claz [VarT man]
    let ctxs' = map helper ctxs
    let y' = ForallT [PlainTV man] ctxs' y
    return [tySyn, x, {-SigD yname y',-} z]
