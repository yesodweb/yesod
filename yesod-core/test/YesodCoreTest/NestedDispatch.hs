{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module YesodCoreTest.NestedDispatch
    ( specs
    , Widget
    , resourcesApp
    ) where

import Data.Foldable (for_)
import Yesod.Core
import Test.Hspec
import Test.Hspec.Expectations.Contrib (annotate)
import Network.Wai
import Network.Wai.Test
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromJust)
import Data.Monoid (Endo (..))
import qualified Control.Monad.Trans.Writer    as Writer
import qualified Data.Set as Set
import YesodCoreTest.NestedDispatch.Resources
import YesodCoreTest.NestedDispatch.NestR (NestR(..))
import YesodCoreTest.NestedDispatch.ParentR (ParentR(..))
import YesodCoreTest.NestedDispatch.Parent0R (Parent0R(..))
import YesodCoreTest.NestedDispatch.Parent0R.Child0R
import qualified Network.HTTP.Types as H

mkYesod "App" nestedDispatchResources

instance Yesod App where
    messageLoggerSource = mempty

specialHtml :: IsString a => a
specialHtml = "text/html; charset=special"

tshow :: Show a => a -> Text
tshow = Text.pack . show

handleRobotsIndexR :: HandlerFor site Text
handleRobotsIndexR = pure "robots.txt"

getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
    rep typeHtml "HTML"
    rep specialHtml "HTMLSPECIAL"
    rep typeXml "XML"
    rep typeJson "JSON"

getNestFooR :: Handler ()
getNestFooR = pure ()

rep :: Monad m => ContentType -> Text -> Writer.Writer (Data.Monoid.Endo [ProvidedRep m]) ()
rep ct t = provideRepType ct $ return (t :: Text)

getJsonR :: Handler TypedContent
getJsonR = selectRep $ do
  rep typeHtml "HTML"
  provideRep $ return $ object ["message" .= ("Invalid Login" :: Text)]

testRequest :: Int -- ^ http status code
            -> Request
            -> ByteString -- ^ expected body
            -> Spec
testRequest status req expected = it (S8.unpack $ fromJust $ lookup "Accept" $ requestHeaders req) $ do
    testRequestIO status req (Just expected)

testRequestIO :: HasCallStack => Int -- ^ http status code
            -> Request
            -> Maybe ByteString -- ^ expected body
            -> IO ()
testRequestIO status req mexpected = do
    app <- toWaiApp App
    sres <- flip runSession app $ do
        request req
    annotate ("Request body: " <> show (simpleBody sres )) $ do
        H.statusCode (simpleStatus sres) `shouldBe` status
        for_ mexpected $ \expected -> do
            simpleBody sres `shouldBe` expected

test :: String -- ^ accept header
     -> ByteString -- ^ expected body
     -> Spec
test accept expected =
    testRequest 200 (acceptRequest accept) expected

acceptRequest :: String -> Request
acceptRequest accept = defaultRequest
            { requestHeaders = [("Accept", S8.pack accept)]
            }


specs :: Spec
specs = do
    describe "Dispatch" $ do
        describe "properly does nested dispatch" $ do
            describe "NestR" $ do
                it "GET" $ do
                    testRequestIO
                        200
                        defaultRequest
                            { pathInfo = ["nest"]
                            , requestMethod = "GET"
                            }
                        (Just "getNestIndexR")
                it "POST" $ do
                    testRequestIO
                        200
                        defaultRequest
                            { pathInfo = ["nest"]
                            , requestMethod = "POST"
                            }
                        (Just "hello")
                it "invalid route" $ do
                    testRequestIO
                        404
                        defaultRequest
                            { pathInfo = ["nest", "oops"]
                            , requestMethod = "GET"
                            }
                        Nothing
                it "invalid method" $ do
                    testRequestIO
                        405
                        defaultRequest
                            { pathInfo = ["nest"]
                            , requestMethod = "PUT"
                            }
                        Nothing

            describe "ParentR" $ do
                describe "Child1R" $ do
                    it "works" $ do
                        testRequestIO
                            200
                            defaultRequest
                                { pathInfo =
                                    ["parent", tshow @Int 1, "hello", "child1"]
                                }
                            (Just "1hello")

                describe "Child2R" $ do
                    it "GET" $ do
                        testRequestIO
                            200
                            defaultRequest
                                { pathInfo =
                                    ["parent", tshow @Int 1, tshow @Int 3, "child2"]
                                , requestMethod = "GET"
                                }
                            (Just "GET(1,3)")
                    it "POST" $ do
                        testRequestIO
                            200
                            defaultRequest
                                { pathInfo =
                                    ["parent", tshow @Int 1, tshow @Int 3, "child2"]
                                , requestMethod = "POST"
                                }
                            (Just "POST(1,3)")
                    it "PUT" $ do
                        testRequestIO
                            405
                            defaultRequest
                                { pathInfo =
                                    ["parent", tshow @Int 1, tshow @Int 3, "child2"]
                                , requestMethod = "PUT"
                                }
                            Nothing

            describe "Parent0R" $ do
                describe "Parent0IndexR" $ do
                    it "GET works" $ do
                        testRequestIO
                            200
                            defaultRequest
                                { pathInfo =
                                    ["parent0", tshow @Int 1]
                                , requestMethod = "GET"
                                }
                            (Just "1")
                    it "POST errors" $ do
                        testRequestIO
                            405
                            defaultRequest
                                { pathInfo =
                                    ["parent0", tshow @Int 1]
                                , requestMethod = "POST"
                                }
                            Nothing

                describe "Child0R" $ do
                    describe "ParentChildIndexR" $ do
                        it "GET works" $ do
                            testRequestIO
                                200
                                defaultRequest
                                    { pathInfo =
                                        ["parent0", tshow @Int 1, "child0", "asdf"]
                                    , requestMethod = "GET"
                                    }
                                (Just "(1,\"asdf\")")
                        it "POST works" $ do
                            testRequestIO
                                200
                                defaultRequest
                                    { pathInfo =
                                        ["parent0", tshow @Int 2, "child0", "asdf"]
                                    , requestMethod = "POST"
                                    }
                                (Just "(2,\"asdf\")")
                        it "PUT errors" $ do
                            testRequestIO
                                405
                                defaultRequest
                                    { pathInfo =
                                        ["parent0", tshow @Int 2, "child0", "asdf"]
                                    , requestMethod = "PUT"
                                    }
                                Nothing

                    describe "ParentChildR" $ do
                        it "GET works" $ do
                            testRequestIO
                                200
                                defaultRequest
                                    { pathInfo =
                                        ["parent0", tshow @Int 1, "child0", "asdf", "foobar"]
                                    , requestMethod = "GET"
                                    }
                                (Just "(1,\"asdf\",\"foobar\")")
                        it "POST works" $ do
                            testRequestIO
                                200
                                defaultRequest
                                    { pathInfo =
                                        ["parent0", tshow @Int 1, "child0", "asdf", "foobar"]
                                    , requestMethod = "POST"
                                    }
                                (Just "(1,\"asdf\",\"foobar\")")
                        it "PUT fails" $ do
                            testRequestIO
                                405
                                defaultRequest
                                    { pathInfo =
                                        ["parent0", tshow @Int 1, "child0", "asdf", "foobar"]
                                    , requestMethod = "PUT"
                                    }
                                Nothing

    describe "UrlToDispatch NestIndexR" $ do
        it "works" $ do
            yre <- mkYesodRunnerEnv App
            let app = urlToDispatch NestIndexR yre
                req = defaultRequest
                    { pathInfo = ["nest"]
                    , requestMethod = "GET"
                    }
            sres <- flip runSession app $ do
                request req
            H.statusCode (simpleStatus sres) `shouldBe` 200
            simpleBody sres `shouldBe` "getNestIndexR"


    describe "selectRep" $ do
        test "application/json" "JSON"
        test (S8.unpack typeJson) "JSON"
        test "text/xml" "XML"
        test (S8.unpack typeXml) "XML"
        test "text/xml,application/json" "XML"
        test "text/xml;q=0.9,application/json;q=1.0" "JSON"
        test (S8.unpack typeHtml) "HTML"
        test "text/html" "HTML"
        test specialHtml "HTMLSPECIAL"
        testRequest 200 (acceptRequest "application/json") { pathInfo = ["json"] } "{\"message\":\"Invalid Login\"}"
        test "text/*" "HTML"
        test "*/*" "HTML"
    describe "routeAttrs" $ do
        it "HomeR" $ routeAttrs HomeR `shouldBe` Set.singleton "home"
        it "JsonR" $ routeAttrs JsonR `shouldBe` Set.empty
        it "ChildR" $ routeAttrs (ParentR 5 $ Child1R "ignored") `shouldBe` Set.singleton "child"

    describe "fallthrough" $ do
        it "is 404 because fallthrough is disabled" $ do
            testRequestIO
                404
                defaultRequest
                    { pathInfo = ["nest", "foo"]
                    , requestMethod = "GET"
                    }
                Nothing

<<<<<<< Updated upstream
=======
    describe "BlahR" $ do
        -- This test is to verify that you can promote a singleton route-
        -- ie, that
        --
        -- > /blah BlahR GET
        --
        -- and
        --
        -- > /blah BlahR:
        -- >     / BlahIndexR GET
        --
        -- are equivalent
        it "can access with just the prefix" $ do
            testRequestIO
                200
                defaultRequest
                    { pathInfo = ["blah"]
                    , requestMethod = "GET"
                    }
                (Just "getBlahIndexR")

        it "can access a filename route" $ do
            testRequestIO
                200
                defaultRequest
                    { pathInfo = ["robots.txt"]
                    , requestMethod = "GET"
                    }
                (Just "robots.txt")

>>>>>>> Stashed changes
    describe "ToParentRoute" $ do
        describe "Route App" $ do
            it "works with no args" $ do
                toParentRoute () HomeR `shouldBe` HomeR
            it "works with args" $ do
                toParentRoute () (ParentR 1 (Child1R "hello"))
                    `shouldBe`
                        (ParentR 1 (Child1R "hello"))
        describe "ParentR" $ do
            it "works" $ do
                toParentRoute 3 (Child1R "Hello")
                    `shouldBe`
                        ParentR 3 (Child1R "Hello")

        describe "Parent0R" $ do
            it "works" $ do
                toParentRoute 3 Parent0IndexR
                    `shouldBe`
                        Parent0R 3 Parent0IndexR

            describe "Child0R" $ do
                it "works" $ do
                    toParentRoute (3, "hello") ParentChildIndexR
                        `shouldBe`
                            Parent0R 3 (Child0R "hello" ParentChildIndexR)

                it "works with arg" $ do
                    toParentRoute (3, "hello") (ParentChildR "asdf")
                        `shouldBe`
                            Parent0R 3 (Child0R "hello" (ParentChildR "asdf"))
