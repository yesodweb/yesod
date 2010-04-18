{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Yesod.Resource
    ( parseRoutes
    , mkYesod
    , mkYesodSub
    ) where

import Web.Routes.Quasi (parseRoutes, createRoutes, Resource (..))
import Yesod.Handler
import Language.Haskell.TH.Syntax
import Yesod.Yesod

mkYesod :: String -> [Resource] -> Q [Dec]
mkYesod name res = do
    let name' = mkName name
    let tySyn = TySynInstD ''Routes [ConT $ name'] (ConT $ mkName $ name ++ "Routes")
    let gsbod = NormalB $ VarE $ mkName $ "site" ++ name ++ "Routes"
    let yes' = FunD (mkName "getSite") [Clause [] gsbod []]
    let yes = InstanceD [] (ConT ''YesodSite `AppT` ConT name') [yes']
    decs <- createRoutes (name ++ "Routes")
                         (ConT ''YesodApp)
                         name'
                         "runHandler'"
                         res
    return $ tySyn : yes : decs

mkYesodSub :: String -> [Resource] -> Q [Dec]
mkYesodSub name res = do
    let name' = mkName name
    let tySyn = TySynInstD ''Routes [ConT $ name'] (ConT $ mkName $ name ++ "Routes")
    let yas = ConT ''YesodApp `AppT` VarT (mkName "master")
    decs <- createRoutes (name ++ "Routes")
                         yas
                         name'
                         "runHandlerSub"
                         res
    return $ tySyn : decs
