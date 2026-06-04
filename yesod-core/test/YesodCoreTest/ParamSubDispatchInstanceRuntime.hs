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

-- | Runtime coverage for 'mkYesodSubDispatchInstance' (the public convenience
-- that generates both the @YesodSubDispatch@ instance and the
-- @YesodSubDispatchNested@ instances for a parameterized subsite). The
-- function had **no** in-repo test before; this exercises the happy path —
-- a parameterized subsite whose subroutes are parameterized
-- ('setParameterizedSubroute') — end to end (compile + WAI dispatch + route
-- round-trip).
--
-- The misuse case (parameterized subsite + unparameterized nested datatype)
-- is a compile error by design, so it is covered by the pure unit test for
-- 'checkNestedSubArity' in @Route.SubDispatchAritySpec@ rather than here.
module YesodCoreTest.ParamSubDispatchInstanceRuntime
    ( specs
    ) where

import Data.Text (Text)
import Test.Hspec
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types as H
import Yesod.Core
import Yesod.Core.Dispatch (toWaiApp, mkYesodSubDispatchInstance)

import YesodCoreTest.ParamSubsiteParameterized
import YesodCoreTest.RuntimeHarness (assertRequest)

-- | Concrete instantiation of the parameterized subsite.
data ConcretePSub = ConcretePSub

instance PClass ConcretePSub App where
    type PAssoc ConcretePSub = Text
    pValue _ _ = "value"

type ConcretePSubT = PSub ConcretePSub

data App = App

-- Handlers for the subsite. The nested handlers receive the parent's
-- 'PAssoc' dynamic, exactly as for any other nested route.
getPHomeR :: SubHandlerFor (PSub subsite) master Text
getPHomeR = pure "pHome"

getPItemR :: Int -> SubHandlerFor (PSub subsite) master Text
getPItemR _ = pure "pItem"

getPNestedHomeR :: PAssoc subsite -> SubHandlerFor (PSub subsite) master Text
getPNestedHomeR _ = pure "pNestedHome"

getPNestedDetailR :: PAssoc subsite -> Int -> SubHandlerFor (PSub subsite) master Text
getPNestedDetailR _ _ = pure "pNestedDetail"

-- The API under test: generate the YesodSubDispatch + YesodSubDispatchNested
-- instances for the parameterized subsite. Placed before 'mkYesod' so the
-- master dispatch can see the generated subsite instances.
mkYesodSubDispatchInstance "(PClass subsite master) => PSub subsite" resourcesPSub

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
testRequestIO status path method mexpected =
    assertRequest (toWaiApp App) method status path mexpected

specs :: Spec
specs = describe "mkYesodSubDispatchInstance (parameterized subsite, opt-in subroutes)" $ do
    describe "parseRoute . renderRoute round-trips" $
        mapM_
            (\r -> it (show r) $ parseRoute (renderRoute r) `shouldBe` Just r)
            sampleRoutes

    describe "WAI dispatch (subsite mounted at /p)" $ do
        it "static leaf (PHomeR)" $
            testRequestIO 200 ["p"] "GET" (Just "pHome")
        it "dynamic leaf (PItemR)" $
            testRequestIO 200 ["p", "item", "7"] "GET" (Just "pItem")
        it "nested parent home (PNestedR _ PNestedHomeR)" $
            testRequestIO 200 ["p", "abc", "nested"] "GET" (Just "pNestedHome")
        it "nested parent dynamic leaf (PNestedR _ (PNestedDetailR _))" $
            testRequestIO 200 ["p", "abc", "nested", "detail", "9"] "GET" (Just "pNestedDetail")
        it "404 on unknown nested suffix" $
            testRequestIO 404 ["p", "abc", "nested", "oops"] "GET" Nothing
        it "405 on wrong method" $
            testRequestIO 405 ["p", "abc", "nested"] "POST" Nothing
