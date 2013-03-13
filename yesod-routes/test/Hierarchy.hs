{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Hierarchy
    ( hierarchy
    , Dispatcher (..)
    , runHandler
    , Handler
    , App
    , toText
    , Env (..)
    , subDispatch
    ) where

import Test.Hspec
import Test.HUnit
import Yesod.Routes.Parse
import Yesod.Routes.TH
import Yesod.Routes.Class
import Language.Haskell.TH.Syntax
import qualified Yesod.Routes.Class as YRC
import Data.Text (Text, pack, append)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8

class ToText a where
    toText :: a -> Text

instance ToText Text where toText = id
instance ToText String where toText = pack

type Handler sub master a = a
type Request = ([Text], ByteString) -- path info, method
type App sub master = Request -> (Text, Maybe (YRC.Route master))
data Env sub master = Env
    { envToMaster :: YRC.Route sub -> YRC.Route master
    , envSub :: sub
    , envMaster :: master
    }

subDispatch
    :: (Env sub master -> App sub master)
    -> (Handler sub master Text -> Env sub master -> Maybe (YRC.Route sub) -> App sub master)
    -> (master -> sub)
    -> (YRC.Route sub -> YRC.Route master)
    -> Env master master
    -> App sub master
subDispatch handler _runHandler getSub toMaster env req =
    handler env' req
  where
    env' = env
        { envToMaster = envToMaster env . toMaster
        , envSub = getSub $ envMaster env
        }

class Dispatcher sub master where
    dispatcher :: Env sub master -> App sub master

runHandler
    :: ToText a
    => Handler sub master a
    -> Env sub master
    -> Maybe (Route sub)
    -> App sub master
runHandler h Env {..} route _ = (toText h, fmap envToMaster route)


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
    dispatch <- mkDispatchClause MkDispatchSettings
        { mdsRunHandler = [|runHandler|]
        , mdsSubDispatcher = [|subDispatch|]
        , mdsGetPathInfo = [|fst|]
        , mdsMethod = [|snd|]
        , mdsSetPathInfo = [|\p (_, m) -> (p, m)|]
        , mds404 = [|pack "404"|]
        , mds405 = [|pack "405"|]
        } resources
    return
        $ InstanceD
            []
            (ConT ''Dispatcher
                `AppT` ConT ''Hierarchy
                `AppT` ConT ''Hierarchy)
            [FunD (mkName "dispatcher") [dispatch]]
        : rrinst

getHomeR :: Handler sub master String
getHomeR = "home"

getAdminRootR :: Int -> Handler sub master Text
getAdminRootR i = pack $ "admin root: " ++ show i

getLoginR :: Int -> Handler sub master Text
getLoginR i = pack $ "login: " ++ show i

postLoginR :: Int -> Handler sub master Text
postLoginR i = pack $ "post login: " ++ show i

getTableR :: Int -> Text -> Handler sub master Text
getTableR _ t = append "TableR " t

hierarchy :: Spec
hierarchy = describe "hierarchy" $ do
    it "renders root correctly" $
        renderRoute (AdminR 5 AdminRootR) @?= (["admin", "5"], [])
    it "renders table correctly" $
        renderRoute (AdminR 6 $ TableR "foo") @?= (["admin", "6", "table", "foo"], [])
    let disp m ps = dispatcher
            (Env
                { envToMaster = id
                , envMaster = Hierarchy
                , envSub = Hierarchy
                })
            (map pack ps, S8.pack m)
    it "dispatches root correctly" $ disp "GET" ["admin", "7"] @?= ("admin root: 7", Just $ AdminR 7 AdminRootR)
    it "dispatches table correctly" $ disp "GET" ["admin", "8", "table", "bar"] @?= ("TableR bar", Just $ AdminR 8 $ TableR "bar")
