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
    , RunHandler (..)
    , Handler
    , App
    , toText
    , Env (..)
    , fixEnv
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
    { envRoute :: Maybe (YRC.Route sub)
    , envToMaster :: YRC.Route sub -> YRC.Route master
    , envSub :: sub
    , envMaster :: master
    }

fixEnv :: (oldSub -> newSub)
       -> (YRC.Route newSub -> YRC.Route oldSub)
       -> (YRC.Route oldSub -> Env oldSub master)
       -> (YRC.Route newSub -> Env newSub master)
fixEnv toSub toRoute getEnv newRoute =
    go (getEnv $ toRoute newRoute)
  where
    go env = env
        { envRoute = Just newRoute
        , envToMaster = envToMaster env . toRoute
        , envSub = toSub $ envSub env
        }

class Dispatcher sub master where
    dispatcher
        :: App sub master -- ^ 404 page
        -> (YRC.Route sub -> App sub master) -- ^ 405 page
        -> (YRC.Route sub -> Env sub master)
        -> App sub master

class RunHandler sub master where
    runHandler
        :: ToText a
        => Handler sub master a
        -> Env sub master
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
    dispatch <- mkDispatchClause MkDispatchSettings
        { mdsRunHandler = [|runHandler|]
        , mdsDispatcher = [|dispatcher|]
        , mdsFixEnv = [|fixEnv|]
        , mdsGetPathInfo = [|fst|]
        , mdsMethod = [|snd|]
        , mdsSetPathInfo = [|\p (_, m) -> (p, m)|]
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

instance RunHandler Hierarchy master where
    runHandler h Env {..} _ = (toText h, fmap envToMaster envRoute)

hierarchy :: Spec
hierarchy = describe "hierarchy" $ do
    it "renders root correctly" $
        renderRoute (AdminR 5 AdminRootR) @?= (["admin", "5"], [])
    it "renders table correctly" $
        renderRoute (AdminR 6 $ TableR "foo") @?= (["admin", "6", "table", "foo"], [])
    let disp m ps = dispatcher
            (const (pack "404", Nothing))
            (\route -> const (pack "405", Just route))
            (\route -> Env
                { envRoute = Just route
                , envToMaster = id
                , envMaster = Hierarchy
                , envSub = Hierarchy
                })
            (map pack ps, S8.pack m)
    it "dispatches root correctly" $ disp "GET" ["admin", "7"] @?= ("admin root: 7", Just $ AdminR 7 AdminRootR)
    it "dispatches table correctly" $ disp "GET" ["admin", "8", "table", "bar"] @?= ("TableR bar", Just $ AdminR 8 $ TableR "bar")
