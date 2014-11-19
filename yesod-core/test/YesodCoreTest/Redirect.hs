{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, OverloadedStrings #-}
module YesodCoreTest.Redirect (specs, Widget) where

import YesodCoreTest.YesodTest
import Yesod.Core.Handler (redirectWith, setEtag)
import qualified Network.HTTP.Types as H

data Y = Y
mkYesod "Y" [parseRoutes|
/ RootR GET POST
/r301 R301 GET
/r303 R303 GET
/r307 R307 GET
/rregular RRegular GET
/etag EtagR GET
|]
instance Yesod Y where approot = ApprootStatic "http://test"
app :: Session () -> IO ()
app = yesod Y

getRootR :: Handler ()
getRootR = return ()

postRootR :: Handler ()
postRootR = return ()

getR301, getR303, getR307, getRRegular, getEtagR :: Handler ()
getR301 = redirectWith H.status301 RootR
getR303 = redirectWith H.status303 RootR
getR307 = redirectWith H.status307 RootR
getRRegular = redirect RootR
getEtagR = setEtag "hello world"

specs :: Spec
specs = describe "Redirect" $ do
    it "no redirect" $ app $ do
      res <- request defaultRequest { pathInfo = [], requestMethod = "POST" }
      assertStatus 200 res
      assertBodyContains "" res

    it "301 redirect" $ app $ do
      res <- request defaultRequest { pathInfo = ["r301"] }
      assertStatus 301 res
      assertBodyContains "" res

    it "303 redirect" $ app $ do
      res <- request defaultRequest { pathInfo = ["r303"] }
      assertStatus 303 res
      assertBodyContains "" res

    it "307 redirect" $ app $ do
      res <- request defaultRequest { pathInfo = ["r307"] }
      assertStatus 307 res
      assertBodyContains "" res

    it "303 redirect for regular, HTTP 1.1" $ app $ do
      res <- request defaultRequest {
        pathInfo = ["rregular"],
        httpVersion = H.http11
      }
      assertStatus 303 res
      assertBodyContains "" res
    it "302 redirect for regular, HTTP 1.0" $ app $ do
      res <- request defaultRequest {
        pathInfo = ["rregular"]
      , httpVersion = H.http10
      }
      assertStatus 302 res
      assertBodyContains "" res

    describe "etag" $ do
      it "no if-none-match" $ app $ do
        res <- request defaultRequest { pathInfo = ["etag"] }
        assertStatus 200 res
        assertHeader "etag" "\"hello world\"" res
      it "single, unquoted if-none-match" $ app $ do
        res <- request defaultRequest
            { pathInfo = ["etag"]
            , requestHeaders = [("if-none-match", "hello world")]
            }
        assertStatus 304 res
      it "different if-none-match" $ app $ do
        res <- request defaultRequest
            { pathInfo = ["etag"]
            , requestHeaders = [("if-none-match", "hello world!")]
            }
        assertStatus 200 res
        assertHeader "etag" "\"hello world\"" res
      it "single, quoted if-none-match" $ app $ do
        res <- request defaultRequest
            { pathInfo = ["etag"]
            , requestHeaders = [("if-none-match", "\"hello world\"")]
            }
        assertStatus 304 res
      it "multiple quoted if-none-match" $ app $ do
        res <- request defaultRequest
            { pathInfo = ["etag"]
            , requestHeaders = [("if-none-match", "\"foo\", \"hello world\"")]
            }
        assertStatus 304 res
      it "ignore weak" $ app $ do
        res <- request defaultRequest
            { pathInfo = ["etag"]
            , requestHeaders = [("if-none-match", "\"foo\", W/\"hello world\"")]
            }
        assertStatus 200 res
