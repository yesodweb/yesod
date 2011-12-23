{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, OverloadedStrings #-}
module YesodCoreTest.Redirect (specs) where

import YesodCoreTest.YesodTest
import Yesod.Handler (RedirectType(..))
import qualified Network.HTTP.Types as H

data Y = Y
mkYesod "Y" [parseRoutes|
/ RootR GET
/r301 R301 GET
/r303 R303 GET
/r307 R307 GET
|]
instance Yesod Y where approot _ = "http://test"
app :: Session () -> IO ()
app = yesod Y

getRootR :: Handler ()
getRootR = return ()

getR301, getR303, getR307 :: Handler ()
getR301 = redirect RedirectPermanent RootR
getR303 = redirect RedirectSeeOther RootR
getR307 = redirect RedirectTemporary RootR


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

  , it "302 redirect instead of 307 for http 1.0" $ app $ do
      res <- request defaultRequest {
        pathInfo = ["r307"], httpVersion = H.http10
      }
      assertStatus 302 res
      assertBodyContains "" res
  ]
