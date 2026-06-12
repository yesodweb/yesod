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

-- | Runtime + round-trip coverage for a *parameterized* top-level site whose
-- nested route block is split into its own module with
-- 'setFocusOnNestedRoute'. Before this fixture the
-- @parameterized + setFocusOnNestedRoute@ corner was never compiled by any
-- test, and the focused 'RouteAttrsNested' instance head was built from a bare
-- constructor (no type argument applied) — a guaranteed kind error for a
-- parameterized site. The parent dispatch is generated here with
-- 'setParameterizedSubroute', so it must *delegate* to the @SubR a@ fragment
-- instances compiled in "YesodCoreTest.ParamFocusSplit.SubR"; the build itself
-- is the split-delegation assertion and the WAI rows confirm dispatch.
module YesodCoreTest.ParamFocusSplit.Runtime
    ( specs
    ) where

import Data.Text (Text)
import Test.Hspec
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as H
import Yesod.Core

import YesodCoreTest.ParamFocusSplit.Resources
-- Brings the split 'SubR' datatype + its nested dispatch / parse / render /
-- attrs instances into scope. Importing the datatype (not @SubR ()@) is what
-- lets the parent splice's delegation probe resolve 'SubR' and delegate to the
-- imported instance instead of regenerating (and demanding) its handlers.
import YesodCoreTest.ParamFocusSplit.SubR (SubR (..))
import YesodCoreTest.RuntimeHarness (assertRequestFor)

-- The parent splice: generates the route data types and the parent dispatch.
-- 'setParameterizedSubroute' forces the NestedDiscovery path so the parent
-- delegates to the split 'SubR' fragment rather than inlining it.
mkYesodOpts (setParameterizedSubroute True defaultOpts) "PApp a" paramFocusResources

instance Yesod (PApp a) where
    messageLoggerSource = mempty

getHomeR :: HandlerFor (PApp a) Text
getHomeR = pure "home"

getItemR :: Int -> HandlerFor (PApp a) Text
getItemR _ = pure "item"

sampleRoutes :: [Route (PApp ())]
sampleRoutes =
    [ HomeR
    , ItemR 7
    , SubR SubHomeR
    , SubR (SubDetailR 9)
    ]

testRequestIO
    :: HasCallStack
    => Int
    -> [Text]
    -> H.Method
    -> Maybe L.ByteString
    -> IO ()
testRequestIO status path method mexpected =
    assertRequestFor (PApp :: PApp ()) method status path mexpected

specs :: Spec
specs = describe "parameterized top-level site, nested routes split with setFocusOnNestedRoute" $ do
    describe "parseRoute . renderRoute round-trips" $
        mapM_
            (\r -> it (show r) $ parseRoute (renderRoute r) `shouldBe` Just r)
            sampleRoutes

    describe "WAI dispatch (parent delegates to the split SubR fragment)" $ do
        it "static leaf (HomeR)" $
            testRequestIO 200 [] "GET" (Just "home")
        it "dynamic leaf (ItemR)" $
            testRequestIO 200 ["item", "7"] "GET" (Just "item")
        it "nested parent home (delegated SubR SubHomeR)" $
            testRequestIO 200 ["sub"] "GET" (Just "subHome")
        it "nested parent dynamic leaf (delegated SubR (SubDetailR _))" $
            testRequestIO 200 ["sub", "detail", "9"] "GET" (Just "subDetail")
        it "404 on unknown nested suffix" $
            testRequestIO 404 ["sub", "detail", "9", "oops"] "GET" Nothing
        it "405 on wrong method" $
            testRequestIO 405 ["sub"] "POST" Nothing
