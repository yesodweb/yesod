{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Runtime coverage for a *top-level* parameterized site that opts into
-- nested route discovery ('setParameterizedSubroute'). Existing coverage of
-- this combination ("YesodCoreTest.ParameterizedSite.SubRoute") only fires a
-- single request per route and never round-trips or checks error statuses;
-- this exercises the same opt-in top-level path with full
-- @parseRoute . renderRoute@ round-trips plus 404/405 cases.
module YesodCoreTest.ParamTopLevelRuntime
    ( specs
    ) where

import Data.Text (Text)
import Test.Hspec
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as H
import Yesod.Core

import YesodCoreTest.RuntimeHarness (assertRequestFor)

-- A phantom-parameterized top-level site. With 'setParameterizedSubroute'
-- enabled, the nested subroute datatype 'SubParentR' carries the parameter
-- (kind @Type -> Type@), so this drives the NestedDiscovery codegen path.
data Poly a = Poly a

mkYesodOpts (setParameterizedSubroute True defaultOpts) "Poly a" [parseRoutes|
/ HomeR GET
/item/#Int ItemR GET
/sub SubParentR:
    / SubHomeR GET
    /detail/#Int SubDetailR GET
|]

instance Yesod (Poly a) where
    messageLoggerSource = mempty

getHomeR :: HandlerFor (Poly a) Text
getHomeR = pure "home"

getItemR :: Int -> HandlerFor (Poly a) Text
getItemR _ = pure "item"

getSubHomeR :: HandlerFor (Poly a) Text
getSubHomeR = pure "subHome"

getSubDetailR :: Int -> HandlerFor (Poly a) Text
getSubDetailR _ = pure "subDetail"

sampleRoutes :: [Route (Poly ())]
sampleRoutes =
    [ HomeR
    , ItemR 7
    , SubParentR SubHomeR
    , SubParentR (SubDetailR 9)
    ]

testRequestIO
    :: HasCallStack
    => Int
    -> [Text]
    -> H.Method
    -> Maybe L.ByteString
    -> IO ()
testRequestIO status path method mexpected =
    assertRequestFor (Poly ()) method status path mexpected

specs :: Spec
specs = describe "top-level parameterized site, opted into nested discovery" $ do
    describe "parseRoute . renderRoute round-trips" $
        mapM_
            (\r -> it (show r) $ parseRoute (renderRoute r) `shouldBe` Just r)
            sampleRoutes

    describe "WAI dispatch" $ do
        it "static leaf (HomeR)" $
            testRequestIO 200 [] "GET" (Just "home")
        it "dynamic leaf (ItemR)" $
            testRequestIO 200 ["item", "7"] "GET" (Just "item")
        it "nested parent home (SubParentR SubHomeR)" $
            testRequestIO 200 ["sub"] "GET" (Just "subHome")
        it "nested parent dynamic leaf (SubParentR (SubDetailR _))" $
            testRequestIO 200 ["sub", "detail", "9"] "GET" (Just "subDetail")
        it "404 on unknown nested suffix" $
            testRequestIO 404 ["sub", "detail", "9", "oops"] "GET" Nothing
        it "405 on wrong method" $
            testRequestIO 405 ["sub"] "POST" Nothing
