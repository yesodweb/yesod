{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, OverloadedStrings #-}
module YesodCoreTest.Redirect (specs, Widget) where

import YesodCoreTest.YesodTest
import Yesod.Handler (redirectWith)
import qualified Network.HTTP.Types as H

data Y = Y
mkYesod "Y" [parseRoutes|
/ RootR GET
/r301 R301 GET
/r303 R303 GET
/r307 R307 GET
/rregular RRegular GET
|]
instance Yesod Y where approot = ApprootStatic "http://test"
app :: Session () -> IO ()
app = yesod Y

getRootR :: Handler ()
getRootR = return ()

getR301, getR303, getR307, getRRegular :: Handler ()
getR301 = redirectWith H.status301 RootR
getR303 = redirectWith H.status303 RootR
getR307 = redirectWith H.status307 RootR
getRRegular = redirect RootR

specs :: [Spec]
specs = describe "Redirect" [
    it "301 redirect" $ app $ do
      res <- request defaultRequest { pathInfo = ["r301"] }
      assertStatus 301 res
      assertBodyContains "" res

  , it "303 redirect" $ app $ do
      res <- request defaultRequest { pathInfo = ["r303"] }
      assertStatus 303 res
      assertBodyContains "" res

  , it "307 redirect" $ app $ do
      res <- request defaultRequest { pathInfo = ["r307"] }
      assertStatus 307 res
      assertBodyContains "" res

  , it "303 redirect for regular, HTTP 1.1" $ app $ do
      res <- request defaultRequest {
        pathInfo = ["rregular"]
      }
      assertStatus 303 res
      assertBodyContains "" res
  , it "302 redirect for regular, HTTP 1.0" $ app $ do
      res <- request defaultRequest {
        pathInfo = ["rregular"]
      , httpVersion = H.http10
      }
      assertStatus 302 res
      assertBodyContains "" res
  ]
