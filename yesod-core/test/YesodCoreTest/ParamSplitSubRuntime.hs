{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Runtime coverage for a *parameterized* subsite whose nested routes are
-- /split across modules/ — the combination that had no test before (split was
-- monomorphic-only; parameterized nested-discovery was single-module-only).
--
-- The parent @YesodSubDispatch (PSub subsite) master@ instance is generated
-- here with 'mkYesodSubDispatch', but the nested 'PNestedR' handlers and its
-- 'YesodSubDispatchNested' instance live in the separately compiled
-- "YesodCoreTest.ParamSplitSubNested". Because those handlers are NOT in scope
-- here, this module compiles only if the parent splice *delegates* to the
-- external nested instance rather than inlining it — so the build itself is
-- the split-delegation assertion, and the WAI tests confirm it dispatches.
module YesodCoreTest.ParamSplitSubRuntime
    ( specs
    ) where

import Data.Text (Text)
import Network.Wai (pathInfo, requestMethod)
import Network.Wai.Test
import Test.Hspec
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as H
import Yesod.Core

import YesodCoreTest.ParamSubsiteParameterized
import YesodCoreTest.ParamSplitSubNested ()  -- brings the split nested instance into scope

-- | Concrete instantiation of the parameterized subsite.
data ConcretePSub = ConcretePSub

instance PClass ConcretePSub App where
    type PAssoc ConcretePSub = Text
    pValue _ _ = "value"

type ConcretePSubT = PSub ConcretePSub

data App = App

-- Only the FLAT subsite handlers live here; the nested ones are deliberately
-- absent (see module haddock).
getPHomeR :: SubHandlerFor (PSub subsite) master Text
getPHomeR = pure "pHome"

getPItemR :: Int -> SubHandlerFor (PSub subsite) master Text
getPItemR _ = pure "pItem"

-- The parent dispatch. Generated in THIS module (separately from the nested
-- 'PNestedR' instance), so its 'mkYesodSubDispatch' splice must delegate to the
-- imported 'YesodSubDispatchNested (PNestedR subsite)' instance.
instance PClass subsite master => YesodSubDispatch (PSub subsite) master where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesPSub)

mkYesod "App" [parseRoutes|
/ HomeR GET
/p PSubR ConcretePSubT getP
|]

instance Yesod App where
    messageLoggerSource = mempty

getHomeR :: Handler Text
getHomeR = pure "home"

getP :: App -> ConcretePSubT
getP _ = PSub ConcretePSub

sampleRoutes :: [Route ConcretePSubT]
sampleRoutes =
    [ PHomeR
    , PItemR 7
    , PNestedR "abc" PNestedHomeR
    , PNestedR "abc" (PNestedDetailR 9)
    ]

testRequestIO
    :: HasCallStack
    => Int
    -> [Text]
    -> H.Method
    -> Maybe L.ByteString
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
specs = describe "parameterized subsite, nested routes split across modules" $ do
    describe "parseRoute . renderRoute round-trips" $
        mapM_
            (\r -> it (show r) $ parseRoute (renderRoute r) `shouldBe` Just r)
            sampleRoutes

    describe "WAI dispatch (parent delegates to the split nested instance)" $ do
        it "static leaf (PHomeR)" $
            testRequestIO 200 ["p"] "GET" (Just "pHome")
        it "dynamic leaf (PItemR)" $
            testRequestIO 200 ["p", "item", "7"] "GET" (Just "pItem")
        it "nested parent home (delegated PNestedHomeR)" $
            testRequestIO 200 ["p", "abc", "nested"] "GET" (Just "pNestedHome")
        it "nested parent dynamic leaf (delegated PNestedDetailR)" $
            testRequestIO 200 ["p", "abc", "nested", "detail", "9"] "GET" (Just "pNestedDetail")
        it "404 on unknown nested suffix" $
            testRequestIO 404 ["p", "abc", "nested", "oops"] "GET" Nothing
        it "405 on wrong method" $
            testRequestIO 405 ["p", "abc", "nested"] "POST" Nothing
