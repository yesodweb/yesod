{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module YesodCoreTest.NestedDispatch
    ( specs
    , Widget
    , resourcesApp
    ) where

import Data.Foldable (for_)
import Yesod.Core
import Test.Hspec
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

mkYesod "App" nestedDispatchResources

instance Yesod App where
    messageLoggerSource = mempty

specialHtml :: IsString a => a
specialHtml = "text/html; charset=special"

tshow :: Show a => a -> Text
tshow = Text.pack . show

getHomeR :: Handler TypedContent
getHomeR = selectRep $ do
    rep typeHtml "HTML"
    rep specialHtml "HTMLSPECIAL"
    rep typeXml "XML"
    rep typeJson "JSON"

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
