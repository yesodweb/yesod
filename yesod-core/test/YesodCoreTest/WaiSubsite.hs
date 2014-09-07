{-# LANGUAGE CPP, QuasiQuotes, TemplateHaskell, TypeFamilies, MultiParamTypeClasses, OverloadedStrings #-}
module YesodCoreTest.WaiSubsite (specs, Widget) where

import YesodCoreTest.YesodTest
import Yesod.Core
import qualified Network.HTTP.Types as H

myApp :: Application
myApp _ f = f $ responseLBS H.status200 [("Content-type", "text/plain")] "WAI"

getApp :: a -> WaiSubsite
getApp _ = WaiSubsite myApp

data Y = Y
mkYesod "Y" [parseRoutes|
/ RootR GET
/sub WaiSubsiteR WaiSubsite getApp
|]

instance Yesod Y

app :: Session () -> IO ()
app = yesod Y

getRootR :: Handler ()
getRootR = return ()

specs :: Spec
specs = describe "WaiSubsite" $ do
    it "root" $ app $ do
      res <- request defaultRequest { pathInfo = [] }
      assertStatus 200 res
      assertBodyContains "" res

    it "subsite" $ app $ do
      res <- request defaultRequest { pathInfo = ["sub", "foo"] }
      assertStatus 200 res
      assertBodyContains "WAI" res
