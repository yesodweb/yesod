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
    let yaname = mkName $ name ++ "YesodApp"
    let ya = TySynD yaname [] $ ConT ''YesodApp `AppT` ConT name'
    let tySyn = TySynInstD ''Routes [ConT $ name'] (ConT $ mkName $ name ++ "Routes")
    let hand = TySynD (mkName $ name ++ "Handler") [PlainTV $ mkName "a"]
             $ ConT ''Handler `AppT` ConT name' `AppT` VarT (mkName "a")
    let gsbod = NormalB $ VarE $ mkName $ "site" ++ name ++ "Routes"
    let yes' = FunD (mkName "getSite") [Clause [] gsbod []]
    let yes = InstanceD [] (ConT ''YesodSite `AppT` ConT name') [yes']
    decs <- createRoutes (name ++ "Routes")
                         yaname
                         name'
                         "runHandler"
                         res
    return $ ya : tySyn : hand : yes : decs
