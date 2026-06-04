{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Runtime coverage for the backwards-compatible (opt-out) inline
-- nested-route codegen.
--
-- The modules "YesodCoreTest.ParameterizedSubData" /
-- "YesodCoreTest.ParameterizedSubDispatch" only exercise the parameterized
-- /opt-out/ path at *compile time* (data + dispatch instances, no runtime
-- assertions). This module gives the same shape a concrete instantiation and
-- actually exercises it at runtime:
--
--   * 'parseRoute' . 'renderRoute' round-trips for every constructor, and
--   * WAI dispatch returns the right status + body for each path,
--
-- covering the static, dynamic, and (dynamically-keyed) nested-parent inline
-- arms — the path-piece ordering, parent-constructor nesting and @dyns@
-- handling that previously had no runtime guard.
module YesodCoreTest.ParameterizedSubDispatchRuntime
    ( specs
    ) where

import Data.Text (Text)
import Network.Wai (defaultRequest, pathInfo, requestMethod)
import Network.Wai.Test
import Test.Hspec
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as H
import Yesod.Core
import Yesod.Core.Dispatch (toWaiApp)

import YesodCoreTest.ParameterizedSubData
import YesodCoreTest.ParameterizedSubDispatch ()
import YesodCoreTest.ParameterizedSubDispatchRuntime.Data

-- | A concrete subsite for 'ParamSubsite'. Its associated 'AssocType' is
-- 'Text', so the dynamically-keyed nested parent (@!/#{AssocType subsite}@)
-- becomes a plain @Text@ path piece at runtime.
data ConcreteSub = ConcreteSub

instance ParamSubsiteClass ConcreteSub App where
    type AssocType ConcreteSub = Text
    getSubsiteValue _ _ = "concrete"

-- | parseRoutes can only embed a single-token subsite type, so we alias the
-- fully-applied subsites here.
type ConcreteParamSub = ParamSubsite ConcreteSub
type ConcreteBigSub = BigSub ()

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/sub ParamSubR ConcreteParamSub getSub
/big BigSubR ConcreteBigSub getBig
|]

instance Yesod App where
    messageLoggerSource = mempty

getHomeR :: Handler Text
getHomeR = pure "home"

getSub :: App -> ConcreteParamSub
getSub _ = ParamSubsite ConcreteSub

getBig :: App -> ConcreteBigSub
getBig _ = BigSub ()

-- Dispatch + handlers for the BigSub / ChildSub subsites.
instance YesodSubDispatch (BigSub a) master where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesBigSub)

instance YesodSubDispatch ChildSub master where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChildSub)

getBigItemR :: Int -> SubHandlerFor (BigSub a) master Text
getBigItemR _ = pure "bigItem"

getBigMultiR :: [Text] -> SubHandlerFor (BigSub a) master Text
getBigMultiR _ = pure "bigMulti"

getWrapHomeR :: Int -> SubHandlerFor (BigSub a) master Text
getWrapHomeR _ = pure "wrapHome"

getWrapDetailR :: Int -> Int -> SubHandlerFor (BigSub a) master Text
getWrapDetailR _ _ = pure "wrapDetail"

getChildHomeR :: SubHandlerFor ChildSub master Text
getChildHomeR = pure "childHome"

getChildXR :: Int -> SubHandlerFor ChildSub master Text
getChildXR _ = pure "childX"

-- | Every constructor of the parameterized subsite's route, instantiated at
-- the concrete subsite.
sampleRoutes :: [Route ConcreteParamSub]
sampleRoutes =
    [ ParamSubHomeR
    , ParamSubItemR 7
    , NestedR "abc" NestedHomeR
    , NestedR "abc" (NestedDetailR 9)
    ]

-- | Routes for 'BigSub', covering a multipiece leaf and a subsite leaf nested
-- under a 'ResourceParent'.
bigSampleRoutes :: [Route ConcreteBigSub]
bigSampleRoutes =
    [ BigItemR 3
    , BigMultiR ["a", "b", "c"]
    , BigMultiR []
    , WrapR 5 WrapHomeR
    , WrapR 5 (WrapDetailR 9)
    , EmbedParentR (ChildR ChildHomeR)
    , EmbedParentR (ChildR (ChildXR 9))
    ]

testRequestIO
    :: HasCallStack
    => Int               -- ^ expected http status code
    -> [Text]            -- ^ pathInfo
    -> H.Method          -- ^ request method
    -> Maybe L.ByteString -- ^ expected body (Nothing = don't check)
    -> IO ()
testRequestIO status path method mexpected = do
    app <- toWaiApp App
    sres <- flip runSession app $ request defaultRequest
        { pathInfo = path
        , requestMethod = method
        }
    H.statusCode (simpleStatus sres) `shouldBe` status
    case mexpected of
        Nothing -> pure ()
        Just expected -> simpleBody sres `shouldBe` expected

specs :: Spec
specs = describe "opt-out (backwards-compat) parameterized nested routes" $ do
    describe "parseRoute . renderRoute round-trips" $ do
        mapM_
            (\r -> it (show r) $
                parseRoute (renderRoute r) `shouldBe` Just r)
            sampleRoutes
        mapM_
            (\r -> it (show r) $
                parseRoute (renderRoute r) `shouldBe` Just r)
            bigSampleRoutes

    describe "WAI dispatch (subsite mounted at /sub)" $ do
        it "static leaf (ParamSubHomeR)" $
            testRequestIO 200 ["sub"] "GET" (Just "paramSubHome")
        it "dynamic leaf (ParamSubItemR)" $
            testRequestIO 200 ["sub", "item", "7"] "GET" (Just "paramSubItem")
        it "nested parent home (NestedR _ NestedHomeR)" $
            testRequestIO 200 ["sub", "abc", "nested"] "GET" (Just "nestedHome")
        it "nested parent dynamic leaf (NestedR _ (NestedDetailR _))" $
            testRequestIO 200 ["sub", "abc", "nested", "detail", "9"] "GET" (Just "nestedDetail")
        it "404 on unknown nested suffix" $
            testRequestIO 404 ["sub", "abc", "nested", "oops"] "GET" Nothing
        it "405 on wrong method for nested leaf" $
            testRequestIO 405 ["sub", "abc", "nested"] "POST" Nothing

    describe "WAI dispatch (BigSub mounted at /big)" $ do
        it "dynamic leaf (BigItemR)" $
            testRequestIO 200 ["big", "item", "3"] "GET" (Just "bigItem")
        it "multipiece leaf (BigMultiR)" $
            testRequestIO 200 ["big", "multi", "a", "b", "c"] "GET" (Just "bigMulti")
        it "multipiece leaf, empty tail (BigMultiR [])" $
            testRequestIO 200 ["big", "multi"] "GET" (Just "bigMulti")
        it "nested parent home (WrapR _ WrapHomeR)" $
            testRequestIO 200 ["big", "wrap", "5"] "GET" (Just "wrapHome")
        it "nested parent dynamic leaf (WrapR _ (WrapDetailR _))" $
            testRequestIO 200 ["big", "wrap", "5", "detail", "9"] "GET" (Just "wrapDetail")
        it "subsite leaf nested under parent, home (EmbedParentR (ChildR ChildHomeR))" $
            testRequestIO 200 ["big", "embed", "sub"] "GET" (Just "childHome")
        it "subsite leaf nested under parent, dynamic (EmbedParentR (ChildR (ChildXR _)))" $
            testRequestIO 200 ["big", "embed", "sub", "x", "9"] "GET" (Just "childX")
