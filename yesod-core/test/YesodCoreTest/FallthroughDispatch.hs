{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -ddump-to-file -ddump-splices #-}

module YesodCoreTest.FallthroughDispatch where

import Network.Wai
import Network.Wai.Test
import YesodCoreTest.FallthroughDispatch.Resources
import Yesod.Core
import Test.Hspec
import Data.Foldable (for_)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.Maybe (fromJust)

mkYesodOpts (setNestedRouteFallthrough True defaultOpts) "App" fallthroughDispatchResources

instance Yesod App

handleFooIndexR :: HandlerFor app String
handleFooIndexR = pure "FooIndexR"

handleOtherR :: HandlerFor app String
handleOtherR = pure "OtherR"

handleFooNeatR :: HandlerFor app String
handleFooNeatR = pure "FooNeatR"

handleNeverFiresR :: HandlerFor app String
handleNeverFiresR = pure "NeverFiresR"

handleFooWithIntR :: Int -> HandlerFor app String
handleFooWithIntR i = pure $ "FooWithIntR " <> show i

handleFooWithTextR :: String -> HandlerFor app String
handleFooWithTextR str = pure $ "FooWithTextR " <> str

testRequest :: Int -- ^ http status code
            -> Request
            -> ByteString -- ^ expected body
            -> Spec
testRequest status req expected = it (S8.unpack $ fromJust $ lookup "Accept" $ requestHeaders req) $ do
    testRequestIO status req (Just expected)

testRequestIO :: Int -- ^ http status code
            -> Request
            -> Maybe ByteString -- ^ expected body
            -> IO ()
testRequestIO status req mexpected = do
    app <- toWaiApp App
    flip runSession app $ do
        sres <- request req
        assertStatus status sres
        for_ mexpected $ \expected -> do
            assertBody expected sres

spec :: Spec
spec = do
    describe "FirstFooR" $ do
        it "FooIndexR" $ do
            testRequestIO
                200
                defaultRequest
                    { pathInfo = ["foo"]
                    }
                (Just "FooIndexR")

        it "FooNeatR" $ do
            testRequestIO
                200
                defaultRequest
                    { pathInfo = ["foo", "neat"]
                    }
                (Just "FooNeatR")

    describe "SecondFooR" $ do
        -- This is a little unsatisfying. Ideally we could write a test
        -- that expresses that, uh, `renderRoute (SecondFooR NeverFiresR)`
        -- would always direct to a different route. Difficult to do
        -- though.
        it "NeverFiresR" $ do
            testRequestIO
                200
                defaultRequest
                    { pathInfo = ["foo"]
                    }
                (Just "FooIndexR")

        it "OtherR" $ do
            testRequestIO
                200
                defaultRequest
                    { pathInfo = ["foo", "other"]
                    }
                (Just "OtherR")

    describe "ParamFooR" $ do
        it "FooWithIntR" $ do
            testRequestIO
                200
                defaultRequest
                    { pathInfo = ["foo", "3"]
                    }
                (Just "FooWithIntR 3")

    describe "TextFooR" $ do
        it "FooWithTextR" $ do
            testRequestIO
                200
                defaultRequest
                    { pathInfo = ["foo", "asdf"]
                    }
                (Just "FooWithTextR asdf")
