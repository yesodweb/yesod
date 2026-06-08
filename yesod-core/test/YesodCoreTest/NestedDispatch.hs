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

import Yesod.Core
import Yesod.Core.Class.Dispatch.ToParentRoute (toParentRoute)
import Test.Hspec
import Network.Wai
import Network.Wai.Test
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as S8
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Monoid (Endo (..))
import qualified Control.Monad.Trans.Writer    as Writer
import qualified Data.Set as Set
import YesodCoreTest.NestedDispatch.Resources
import YesodCoreTest.NestedDispatch.NestR (NestR(..))
import YesodCoreTest.NestedDispatch.InnerR (InnerR(..))
import YesodCoreTest.NestedDispatch.ParentR (ParentR(..))
import YesodCoreTest.NestedDispatch.Parent0R (Parent0R(..), Child0R(..))
import YesodCoreTest.NestedDispatch.Parent0R.Child0R
import YesodCoreTest.RuntimeHarness (assertRequestRaw)
import qualified Network.HTTP.Types as H

mkYesod "App" nestedDispatchResources

getBlahIndexR :: HandlerFor App Text
getBlahIndexR = pure "getBlahIndexR"

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
testRequestIO status req mexpected =
    assertRequestRaw (toWaiApp App) req status mexpected

test :: String -- ^ accept header
     -> ByteString -- ^ expected body
     -> Spec
test accept expected =
    testRequest 200 (acceptRequest accept) expected

acceptRequest :: String -> Request
acceptRequest accept = defaultRequest
            { requestHeaders = [("Accept", S8.pack accept)]
            }

-- | Build a WAI app rooted at a nested route fragment (via the public
-- 'toWaiAppPlainNested' entry point), issue one GET at @path@, and return the
-- status code.
nestedStatusFor
    :: (ParentSite route ~ App, YesodDispatchNested route, ToParentRoute route)
    => Proxy route -> ParentArgs route -> [Text] -> IO Int
nestedStatusFor proxy parentArgs path = do
    app <- toWaiAppPlainNested proxy parentArgs App
    sres <- flip runSession app $ request defaultRequest { pathInfo = path }
    pure $ H.statusCode (simpleStatus sres)


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
                describe "FilesR (multi-piece *Texts leaf under a split parent)" $ do
                    it "dispatches a non-empty tail, forwarding parent #Int and the multipiece" $ do
                        testRequestIO
                            200
                            defaultRequest
                                { pathInfo = ["parent0", tshow @Int 7, "files", "a", "b"]
                                , requestMethod = "GET"
                                }
                            (Just "7:[\"a\",\"b\"]")
                    it "dispatches an empty tail as the empty multipiece" $ do
                        testRequestIO
                            200
                            defaultRequest
                                { pathInfo = ["parent0", tshow @Int 7, "files"]
                                , requestMethod = "GET"
                                }
                            (Just "7:[]")
                    it "405s on a non-GET method" $ do
                        testRequestIO
                            405
                            defaultRequest
                                { pathInfo = ["parent0", tshow @Int 7, "files", "a", "b"]
                                , requestMethod = "POST"
                                }
                            Nothing

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

    describe "trailing-slash redirect (cleanPath) for a nested route" $ do
        it "301s a trailing slash on a GET to a split nested route" $ do
            testRequestIO
                301
                defaultRequest
                    { pathInfo = ["parent", tshow @Int 1, tshow @Int 3, "child2", ""]
                    , requestMethod = "GET"
                    }
                Nothing

    describe "nested-dispatch entry points (miss / deep 404)" $ do
        -- These exercise toWaiAppPlainNested/toWaiAppYreNested directly, not just
        -- the happy path. In particular they cover the `Nothing -> notFound`
        -- branch in toWaiAppYreNested (a fragment instance returning Nothing on a
        -- non-match) and confirm a `drop parentDepth` against a too-short pathInfo
        -- yields a clean 404 rather than a mis-dispatch.
        describe "rooted at NestR (no dynamic prefix, depth 1)" $ do
            it "dispatches the matching index path" $
                nestedStatusFor (Proxy :: Proxy NestR) () ["nest"]
                    `shouldReturn` 200
            it "404s a non-matching tail through the Nothing -> notFound branch" $
                nestedStatusFor (Proxy :: Proxy NestR) () ["nest", "oops"]
                    `shouldReturn` 404

        describe "rooted at a deep dynamic parent (ParentR/#Int, depth 2)" $ do
            it "dispatches a full matching path" $
                nestedStatusFor (Proxy :: Proxy ParentR) (1 :: Int)
                    ["parent", "1", "hello", "child1"]
                    `shouldReturn` 200
            it "cleanly 404s a pathInfo shorter than the parent depth" $
                nestedStatusFor (Proxy :: Proxy ParentR) (1 :: Int) ["parent"]
                    `shouldReturn` 404
            it "cleanly 404s an empty pathInfo" $
                nestedStatusFor (Proxy :: Proxy ParentR) (1 :: Int) []
                    `shouldReturn` 404
            it "404s a non-matching deep tail" $
                nestedStatusFor (Proxy :: Proxy ParentR) (1 :: Int)
                    ["parent", "1", "nope"]
                    `shouldReturn` 404

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
        -- Bug-2 regression: InnerR is split (RouteAttrsNested InnerR), OuterR is
        -- inlined. The generated clause must carry the OuterR ancestor pattern.
        it "InnerIndexR (split parent under an inlined parent)" $
            routeAttrs (OuterR (InnerR InnerIndexR)) `shouldBe` Set.singleton "innerleaf"

    describe "OuterR (inlined parent delegating to a split InnerR)" $ do
        it "dispatches /outer/inner" $ do
            testRequestIO
                200
                defaultRequest
                    { pathInfo = ["outer", "inner"]
                    , requestMethod = "GET"
                    }
                (Just "getInnerIndexR")

    describe "fallthrough" $ do
        it "is 404 because fallthrough is disabled" $ do
            testRequestIO
                404
                defaultRequest
                    { pathInfo = ["nest", "foo"]
                    , requestMethod = "GET"
                    }
                Nothing

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
