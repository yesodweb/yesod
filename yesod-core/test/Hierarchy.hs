{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Hierarchy
    ( hierarchy
    , Dispatcher (..)
    , runHandler
    , Handler2
    , App
    , toText
    , Env (..)
    , subDispatch
    -- to avoid warnings
    , deleteDelete2
    , deleteDelete3
    , testRouteDatatype
    ) where

import Test.Hspec
import Test.HUnit
import Yesod.Routes.Parse
import Yesod.Routes.TH
import Yesod.Routes.Class
import Language.Haskell.TH.Syntax
import Data.Text (Text, pack, unpack, append)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Set as Set
import Hierarchy.Admin
import Hierarchy.ResourceTree
import Hierarchy.Nest
import Hierarchy.Nest2
import Hierarchy.Nest3
import Hierarchy.Nest2.NestInner

class ToText a where
    toText :: a -> Text

instance ToText Text where toText = id
instance ToText String where toText = pack

type Handler2 sub master a = a
type Handler site a = Handler2 site site a

type Request = ([Text], ByteString) -- path info, method
type App sub master = Request -> (Text, Maybe (Route master))
data Env sub master = Env
    { envToMaster :: Route sub -> Route master
    , envSub :: sub
    , envMaster :: master
    }

subDispatch
    :: (Env sub master -> App sub master)
    -> (Handler2 sub master Text -> Env sub master -> Maybe (Route sub) -> App sub master)
    -> (master -> sub)
    -> (Route sub -> Route master)
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
    => Handler2 sub master a
    -> Env sub master
    -> Maybe (Route sub)
    -> App sub master
runHandler h Env {..} route _ = (toText h, fmap envToMaster route)

mkRenderRouteInstanceOpts defaultOpts [] [] (ConT ''Hierarchy) hierarchyResourcesWithType

pure <$> mkRouteAttrsInstance [] (ConT ''Hierarchy) hierarchyResourcesWithType

pure <$> mkParseRouteInstance [] (ConT ''Hierarchy) hierarchyResourcesWithType

do
    let resources = hierarchyResources


    rrinst <- mkRenderRouteInstanceOpts defaultOpts [] [] (ConT ''Hierarchy) $ map (fmap parseType) resources
    rainst <- mkRouteAttrsInstance [] (ConT ''Hierarchy) resources
    prinst <- mkParseRouteInstance [] (ConT ''Hierarchy) resources
    (childNames, dispatch) <- mkDispatchClause MkDispatchSettings
        { mdsRunHandler = [|runHandler|]
        , mdsSubDispatcher = [|subDispatch|]
        , mdsGetPathInfo = [|fst|]
        , mdsMethod = [|snd|]
        , mdsSetPathInfo = [|\p (_, m) -> (p, m)|]
        , mds404 = [|pack "404"|]
        , mds405 = [|pack "405"|]
        , mdsGetHandler = defaultGetHandler
        , mdsUnwrapper = return
        , mdsHandleNestedRoute = Nothing
        , mdsNestedRouteFallThrough = False
        } resources
    return $ pure $
        InstanceD
            Nothing
            []
            (ConT ''Dispatcher
                `AppT` ConT ''Hierarchy
                `AppT` ConT ''Hierarchy)
            [FunD (mkName "dispatcher") [dispatch]]

getSpacedR :: Handler site String
getSpacedR = "root-leaf"

getGet2   :: Handler site String; getGet2 = "get"
postPost2 :: Handler site String; postPost2 = "post"
deleteDelete2 :: Int -> Handler site String; deleteDelete2 = const "delete"
getGet3   :: Handler site String; getGet3 = "get"
postPost3 :: Handler site String; postPost3 = "post"
deleteDelete3   :: Int -> Handler site String; deleteDelete3 = const "delete"

getAfter   :: Handler site String; getAfter = "after"

getHomeR :: Handler site String
getHomeR = "home"

getNestInnerIndexR :: Handler site String
getNestInnerIndexR = "inner"

getBackwardsR :: Int -> Handler site Text
getBackwardsR _ = pack "backwards"

getAdminRootR :: Int -> Handler site Text
getAdminRootR i = pack $ "admin root: " ++ show i

getLoginR :: Int -> Handler site Text
getLoginR i = pack $ "login: " ++ show i

postLoginR :: Int -> Handler site Text
postLoginR i = pack $ "post login: " ++ show i

getTableR :: Int -> Text -> Handler site Text
getTableR _ = append "TableR "

getGetPostR :: Handler site Text
getGetPostR = pack "get"

postGetPostR :: Handler site Text
postGetPostR = pack "post"

hierarchy :: Spec
hierarchy = describe "hierarchy" $ do
    it "nested with spacing" $
        renderRoute (NestR SpacedR) @?= (["nest", "spaces"], [])
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

    let testGetPost route getRes postRes = do
          let routeStrs = map unpack $ fst (renderRoute route)
          disp "GET" routeStrs @?= (getRes, Just route)
          disp "POST" routeStrs @?= (postRes, Just route)

    it "dispatches routes with multiple METHODs: admin" $
        testGetPost (AdminR 1 LoginR) "login: 1" "post login: 1"

    it "dispatches routes with multiple METHODs: nesting" $
        testGetPost (NestR $ Nest2 GetPostR) "get" "post"

    it "dispatches root correctly" $ disp "GET" ["admin", "7"] @?= ("admin root: 7", Just $ AdminR 7 AdminRootR)
    it "dispatches table correctly" $ disp "GET" ["admin", "8", "table", "bar"] @?= ("TableR bar", Just $ AdminR 8 $ TableR "bar")
    describe "parseRoute" $ do
        let parseNothing = Nothing :: Maybe (Route Hierarchy)
        describe "HomeR" $ do
            it "works" $ do
                parseRoute ([], []) @?= Just HomeR
            it "with extraneous query params" $ do
                parseRoute ([], [("foo", "bar")]) @?= Just HomeR
        describe "AdminR" $ do
            it "works" $ do
                parseRoute (["admin", "5"], []) @?= Just (AdminR 5 AdminRootR)
            it "fails with extra character" $ do
                parseRoute (["admin!", "5"], []) @?= parseNothing
            it "works with a subroute with param" $ do
                parseRoute (["admin", "6", "table", "hello"], []) @?= Just (AdminR 6 (TableR "hello"))
            describe "parseNestedRoute" $ do
                it "works" $ do
                    parseRouteNested ([], []) @?= Just AdminRootR
                it "works with param" $ do
                    parseRouteNested (["table", "hello"], []) @?= Just (TableR "hello")
        describe "NestR" $ do
            it "fails because there's no top-level handler" $ do
                parseRoute (["nest"], []) @?= parseNothing
            it "works for SpacedR" $ do
                parseRoute (["nest", "spaces"], []) @?= Just (NestR SpacedR)
            it "works with parseRouteNested" $ do
                parseRouteNested (["spaces"], []) @?= Just SpacedR
            describe "Nest2" $ do
                it "works" $ do
                    parseRoute (["nest", "nest2"], []) @?= Just (NestR (Nest2 GetPostR))
                describe "NestInner" $ do
                    it "works" $ do
                        parseRoute (["nest", "nest2", "nest-inner"], []) @?= Just (NestR (Nest2 (NestInner NestInnerIndexR)))
    describe "parseRouteNested" $ do
        describe "NestInner" $ do
            it "works" $ do
                parseRouteNested ([], []) @?= Just NestInnerIndexR
    describe "routeAttrs" $ do
        it "inherited attributes" $ do
            routeAttrs (NestR SpacedR) @?= Set.fromList ["NestingAttr", "NonNested"]
        it "pair attributes" $
            routeAttrs (AfterR After) @?= Set.fromList ["parent", "child", "key=value2"]

        describe "NestingAttr inherits properly" $ do
            it "routeAttrs" $ do
                routeAttrs (NestR (Nest2 GetPostR)) @?= Set.fromList ["NestingAttr"]
            it "routeAttrsNested" $ do
                routeAttrsNested GetPostR @?= Set.fromList ["NestingAttr"]
            it "routeAttrsNested InnerNest" $ do
                routeAttrsNested NestInnerIndexR @?= Set.fromList ["NestingAttr"]


-- This value should compile if all routes are present as expected.
testRouteDatatype :: Route Hierarchy -> Int
testRouteDatatype r =
    case r of
        HomeR -> 0
        BackwardsR _ -> 1
        AdminR _ sub ->
            case sub of
                AdminRootR -> 0
                LoginR -> 0
                TableR _ -> 1
        NestR sub -> testNestR sub
        -- NOTE: This is a bug in the behavior of the parser. See issue
        -- https://github.com/yesodweb/yesod/issues/1886
        --
        -- Nest3, by layout, should be under `NestR`. However, since there
        -- is a comment on column 0, this causes the parser to reset the
        -- column count.
        Nest3 sub ->
            case sub of
                Get3 -> 0
                Post3 -> 0
        AfterR sub ->
            case sub of
                After -> 0

testNestR :: NestR -> Int
testNestR sub =
    case sub of
        SpacedR -> 1
        Nest2 sub' ->
            case sub' of
                GetPostR -> 0
                Get2 -> 0
                Post2 -> 0
                NestInner sub'' ->
                    case sub'' of
                        NestInnerIndexR -> 0
