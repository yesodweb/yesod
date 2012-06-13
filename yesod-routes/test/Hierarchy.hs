{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Hierarchy
    ( hierarchy
    , Dispatcher (..)
    , RunHandler (..)
    , Handler
    , App
    , toText
    ) where

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit
import Yesod.Routes.Parse
import Yesod.Routes.TH
import Yesod.Routes.Class
import Language.Haskell.TH.Syntax
import qualified Yesod.Routes.Class as YRC
import Data.Text (Text, pack, append)

class ToText a where
    toText :: a -> Text

instance ToText Text where toText = id
instance ToText String where toText = pack

type Handler sub master = Text
type App sub master = (Text, Maybe (YRC.Route master))

class Dispatcher sub master where
    dispatcher
        :: master
        -> sub
        -> (YRC.Route sub -> YRC.Route master)
        -> App sub master -- ^ 404 page
        -> (YRC.Route sub -> App sub master) -- ^ 405 page
        -> Text -- ^ method
        -> [Text]
        -> App sub master

class RunHandler sub master where
    runHandler
        :: Handler sub master
        -> master
        -> sub
        -> Maybe (YRC.Route sub)
        -> (YRC.Route sub -> YRC.Route master)
        -> App sub master

data Hierarchy = Hierarchy

do
    let resources = [parseRoutes|
/ HomeR GET
/admin/#Int AdminR:
    / AdminRootR GET
    /login LoginR GET POST
    /table/#Text TableR GET
|]
    rrinst <- mkRenderRouteInstance (ConT ''Hierarchy) $ map (fmap parseType) resources
    dispatch <- mkDispatchClause [|runHandler|] [|dispatcher|] [|toText|] resources
    return
        $ InstanceD
            []
            (ConT ''Dispatcher
                `AppT` ConT ''Hierarchy
                `AppT` ConT ''Hierarchy)
            [FunD (mkName "dispatcher") [dispatch]]
        : rrinst

getHomeR :: Handler sub master
getHomeR = "home"

getAdminRootR :: Int -> Handler sub master
getAdminRootR i = pack $ "admin root: " ++ show i

getLoginR :: Int -> Handler sub master
getLoginR i = pack $ "login: " ++ show i

postLoginR :: Int -> Handler sub master
postLoginR i = pack $ "post login: " ++ show i

getTableR :: Int -> Text -> Handler sub master
getTableR _ t = append "TableR " t

instance RunHandler Hierarchy master where
    runHandler h _ _ subRoute toMaster = (h, fmap toMaster subRoute)

hierarchy :: Specs
hierarchy = describe "hierarchy" $ do
    it "renders root correctly" $
        renderRoute (AdminR 5 AdminRootR) @?= (["admin", "5"], [])
    it "renders table correctly" $
        renderRoute (AdminR 6 $ TableR "foo") @?= (["admin", "6", "table", "foo"], [])
    let disp m ps = dispatcher Hierarchy Hierarchy id (pack "404", Nothing) (\route -> (pack "405", Just route)) (pack m) (map pack ps)
    it "dispatches root correctly" $ disp "GET" ["admin", "7"] @?= ("admin root: 7", Just $ AdminR 7 AdminRootR)
    it "dispatches table correctly" $ disp "GET" ["admin", "8", "table", "bar"] @?= ("TableR bar", Just $ AdminR 8 $ TableR "bar")
