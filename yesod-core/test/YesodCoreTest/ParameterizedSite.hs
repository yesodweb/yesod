{-# LANGUAGE OverloadedStrings #-}

module YesodCoreTest.ParameterizedSite
    ( parameterizedSiteTest
    ) where

import Data.ByteString.Lazy (ByteString)
import Network.Wai.Test (runSession, request, defaultRequest, assertBodyContains, setPath)
import Test.Hspec (Spec, describe, it)
import Yesod.Core (YesodDispatch, renderRoute, Route)
import Yesod.Core.Dispatch (toWaiApp)
import Data.Traversable
import Data.Foldable
import Data.Text.Encoding
import Data.Text as T

import YesodCoreTest.ParameterizedSite.PolyAny (PolyAny (..))
import YesodCoreTest.ParameterizedSite.PolyShow (PolyShow (..))
import YesodCoreTest.ParameterizedSite.Compat (Compat (..))
import qualified YesodCoreTest.ParameterizedSite.SubRoute as SR

-- These are actually tests for template haskell. So if it compiles, it works
parameterizedSiteTest :: Spec
parameterizedSiteTest = describe "Polymorphic Yesod sites" $ do
    it "Polymorphic unconstrained stub" $ runStub (PolyAny ())
    it "Polymorphic stub with Show" $ runStub' "1337" (PolyShow (1337 :: Int))
    it "Polymorphic unconstrained stub, old-style" $ runStub (Compat () ())
    it "Polymorphic stub, sub routes" $ runStubAgainst [(SR.HomeR 456, ["Stub", "123", "456"]), (SR.EditorR (SR.AwayR 789), ["Stub", "123", "789"])] (SR.SubRoute (123 :: Int))

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

runStubAgainst :: YesodDispatch a => [(Route a, [ByteString])] -> a -> IO ()
runStubAgainst urlBodies stub =
    let actions = do
            for_ urlBodies $ \(url, bodies) -> do
                let (nonQuery, _query) = renderRoute url
                let render = T.intercalate "/" nonQuery
                let req = setPath defaultRequest (encodeUtf8 render)
                res <- request req
                for bodies (`assertBodyContains` res)
    in toWaiApp stub >>= runSession actions
