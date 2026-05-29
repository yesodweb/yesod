{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | End-to-end test for 'setUrlToDispatchSiteHook'. The top-level dispatch
-- here is generated with default (hook-free) options, exactly like a
-- production aggregate; it delegates to the focused @YesodDispatchNested@
-- instances (generated /with/ the hook) for each split route. So driving a
-- request through the top-level WAI app — the production shape — must still
-- run the hook, proving the install happens at the shared delegation point
-- rather than only on a test-only path.
module YesodCoreTest.DispatchSiteHook
    ( specs
    ) where

import Test.Hspec
import Data.ByteString.Lazy (ByteString)
import Yesod.Core
import Network.Wai
    ( defaultRequest
    , pathInfo
    , requestMethod
    )
import Network.Wai.Test
    ( runSession
    , request
    , simpleBody
    , simpleStatus
    )
import qualified Network.HTTP.Types as H

import YesodCoreTest.DispatchSiteHook.Common (HookApp(..), hookResources)
import YesodCoreTest.DispatchSiteHook.WidgetsR (WidgetsR(..))
import YesodCoreTest.DispatchSiteHook.OrgR (OrgR(..))

mkYesod "HookApp" hookResources

instance Yesod HookApp where
    messageLoggerSource = mempty

-- | An unset site: any non-@"NO-HOOK"@ body proves the hook overwrote it.
emptySite :: HookApp
emptySite = HookApp Nothing

specs :: Spec
specs = do
    describe "setUrlToDispatchSiteHook" $ do
        describe "production-shaped top-level dispatch (delegating)" $ do
            it "runs the hook for a no-parent route" $ do
                app <- toWaiApp emptySite
                sres <- flip runSession app $ request defaultRequest
                    { pathInfo = ["widgets"]
                    , requestMethod = "GET"
                    }
                H.statusCode (simpleStatus sres) `shouldBe` 200
                simpleBody sres `shouldBe` ("widgets" :: ByteString)

            it "runs the hook for a parent-args route, threading the parent arg" $ do
                app <- toWaiApp emptySite
                sres <- flip runSession app $ request defaultRequest
                    { pathInfo = ["orgs", "42"]
                    , requestMethod = "GET"
                    }
                H.statusCode (simpleStatus sres) `shouldBe` 200
                simpleBody sres `shouldBe` ("org:42" :: ByteString)

        describe "focused urlToDispatch (test-shaped) dispatch" $ do
            it "runs the hook for a no-parent route" $ do
                yre <- mkYesodRunnerEnv emptySite
                let app = urlToDispatch WidgetsIndexR yre
                sres <- flip runSession app $ request defaultRequest
                    { pathInfo = ["widgets"]
                    , requestMethod = "GET"
                    }
                H.statusCode (simpleStatus sres) `shouldBe` 200
                simpleBody sres `shouldBe` ("widgets" :: ByteString)

            it "runs the hook for a parent-args route, threading the parent arg" $ do
                yre <- mkYesodRunnerEnv emptySite
                let app = urlToDispatch (WithParentArgs 42 OrgIndexR) yre
                sres <- flip runSession app $ request defaultRequest
                    { pathInfo = ["orgs", "42"]
                    , requestMethod = "GET"
                    }
                H.statusCode (simpleStatus sres) `shouldBe` 200
                simpleBody sres `shouldBe` ("org:42" :: ByteString)
