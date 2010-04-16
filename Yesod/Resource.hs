{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
module Yesod.Resource
    ( parseRoutes
    , mkYesod
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
                         ''YesodApp
                         name'
                         "runHandler'"
                         res
    return $ tySyn : yes : decs
