{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Delegation-probe regression: a *parameterized* site (@PolyD a@) with a nested route,
-- using /default/ opts (opted out of nested-route discovery), with the route
-- data and the dispatch splice /split across modules/.
--
-- The dispatch splice's @go@ delegation probe must saturate the in-scope child
-- datatype 'SubParentDR' by its own reified arity (0) — NOT by the site's type
-- args — or it builds the ill-kinded @SubParentDR a@ and @isInstance@ throws a
-- kind error that aborts the splice. So this module compiling at all is the
-- regression assertion; the WAI tests confirm the inlined dispatch still works.
module YesodCoreTest.ParamDefaultSplit.Runtime
    ( specs
    ) where

import Data.Text (Text)
import Test.Hspec
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as H
import Yesod.Core

import YesodCoreTest.ParamDefaultSplit.Data
import YesodCoreTest.RuntimeHarness (assertRequestFor)

mkYesodDispatch "PolyD a" resourcesPolyD

instance Yesod (PolyD a) where
    messageLoggerSource = mempty

getHomeDR :: HandlerFor (PolyD a) Text
getHomeDR = pure "home"

getItemDR :: Int -> HandlerFor (PolyD a) Text
getItemDR _ = pure "item"

getSubHomeDR :: HandlerFor (PolyD a) Text
getSubHomeDR = pure "subHome"

getSubDetailDR :: Int -> HandlerFor (PolyD a) Text
getSubDetailDR _ = pure "subDetail"

sampleRoutes :: [Route (PolyD ())]
sampleRoutes =
    [ HomeDR
    , ItemDR 7
    , SubParentDR SubHomeDR
    , SubParentDR (SubDetailDR 9)
    ]

testRequestIO
    :: HasCallStack
    => Int
    -> [Text]
    -> H.Method
    -> Maybe L.ByteString
    -> IO ()
testRequestIO status path method mexpected =
    assertRequestFor (PolyD ()) method status path mexpected

specs :: Spec
specs = describe "parameterized site, default opts, nested route split across modules" $ do
    describe "parseRoute . renderRoute round-trips" $
        mapM_
            (\r -> it (show r) $ parseRoute (renderRoute r) `shouldBe` Just r)
            sampleRoutes

    describe "WAI dispatch (inlined, not delegated)" $ do
        it "static leaf (HomeDR)" $
            testRequestIO 200 [] "GET" (Just "home")
        it "dynamic leaf (ItemDR)" $
            testRequestIO 200 ["item", "7"] "GET" (Just "item")
        it "nested parent home (SubParentDR SubHomeDR)" $
            testRequestIO 200 ["sub"] "GET" (Just "subHome")
        it "nested parent dynamic leaf (SubParentDR (SubDetailDR _))" $
            testRequestIO 200 ["sub", "detail", "9"] "GET" (Just "subDetail")
        it "404 on unknown nested suffix" $
            testRequestIO 404 ["sub", "detail", "9", "oops"] "GET" Nothing
        it "405 on wrong method" $
            testRequestIO 405 ["sub"] "POST" Nothing
