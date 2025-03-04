{-# LANGUAGE OverloadedStrings #-}
module YesodCoreTest.ParameterizedSite
    ( parameterizedSiteTest
    ) where

import Data.ByteString.Lazy (ByteString)
import Network.Wai.Test (runSession, request, defaultRequest, assertBodyContains)
import Test.Hspec (Spec, describe, it)
import Yesod.Core (YesodDispatch)
import Yesod.Core.Dispatch (toWaiApp)

import YesodCoreTest.ParameterizedSite.PolyAny (PolyAny (..))
import YesodCoreTest.ParameterizedSite.PolyShow (PolyShow (..))
import YesodCoreTest.ParameterizedSite.Compat (Compat (..))

-- These are actually tests for template haskell. So if it compiles, it works
parameterizedSiteTest :: Spec
parameterizedSiteTest = describe "Polymorphic Yesod sites" $ do
    it "Polymorphic unconstrained stub" $ runStub (PolyAny ())
    it "Polymorphic stub with Show" $ runStub' "1337" (PolyShow (1337 :: Integer))
    it "Polymorphic unconstrained stub, old-style" $ runStub (Compat () ())

runStub :: YesodDispatch a => a -> IO ()
runStub stub =
    let actions = do
            res <- request defaultRequest
            assertBodyContains "Stub" res
    in toWaiApp stub >>= runSession actions


runStub' :: YesodDispatch a => ByteString -> a -> IO ()
runStub' body stub =
    let actions = do
            res <- request defaultRequest
            assertBodyContains "Stub" res
            assertBodyContains body res
    in toWaiApp stub >>= runSession actions
