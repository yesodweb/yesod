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
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Hierarchy
    ( hierarchy
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

import Yesod.Core
import Test.Hspec
import Test.HUnit
import Yesod.Routes.TH
import Yesod.Routes.Class
import Language.Haskell.TH.Syntax
import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString (ByteString)
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
instance ToText String where toText = Text.pack

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

do
    mconcat
        [ mkRenderRouteInstanceOpts defaultOpts [] [] (ConT ''Hierarchy) hierarchyResourcesWithType
        , pure <$> mkRouteAttrsInstance [] (ConT ''Hierarchy) hierarchyResourcesWithType
        , mkParseRouteInstance [] [] (ConT ''Hierarchy) hierarchyResourcesWithType
        , mkYesodDispatchOpts defaultOpts "Hierarchy" hierarchyResources
        ]

instance Yesod Hierarchy


getAfter :: HandlerFor site Text
getAfter = pure "after"
getHomeR :: HandlerFor site Text
getHomeR = pure "homer"

getBackwardsR :: Int -> HandlerFor site Text
getBackwardsR _i = pure "backwards"

getGet3 :: HandlerFor site Text
getGet3 = pure "getget3"

postPost3 :: HandlerFor site Text
postPost3 = pure "postPost3"

getNestInnerIndexR :: HandlerFor site Text
getNestInnerIndexR = pure "getNestInnerIndexR"

getGetPostR :: HandlerFor site Text
getGetPostR = pure "getGetPostR"

postGetPostR :: HandlerFor site Text
postGetPostR = pure "postGetPostR"

getGet2 :: HandlerFor site Text
getGet2 = pure "getGet2"

postPost2 :: HandlerFor site Text
postPost2 = pure "postPost2"

getSpacedR :: HandlerFor site Text
getSpacedR = pure "getSpacedR"

getAdminRootR :: Int -> HandlerFor site Text
getAdminRootR _ = pure "getAdminRootR"

getLoginR :: Int -> HandlerFor site Text
getLoginR _ = pure "getLoginR"

postLoginR :: Int -> HandlerFor site Text
postLoginR _ = pure "postLoginR"

getTableR :: Int -> Text -> HandlerFor site Text
getTableR _ _ = pure "getTableR"

deleteDelete2 :: Int -> Handler site String; deleteDelete2 = const "delete"
deleteDelete3   :: Int -> Handler site String; deleteDelete3 = const "delete"

hierarchy :: Spec
hierarchy = describe "hierarchy" $ do
    it "nested with spacing" $
        renderRoute (NestR SpacedR) @?= (["nest", "spaces"], [])
    it "renders root correctly" $
        renderRoute (AdminR 5 AdminRootR) @?= (["admin", "5"], [])
    it "renders table correctly" $
        renderRoute (AdminR 6 $ TableR "foo") @?= (["admin", "6", "table", "foo"], [])
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
