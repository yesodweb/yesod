{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.JsLoader (specs) where

import YesodCoreTest.JsLoaderSites.HeadAsync (HA(..))
import YesodCoreTest.JsLoaderSites.Bottom (B(..))

import Test.Hspec
import Test.Hspec.HUnit ()

import Yesod.Core hiding (Request)
import Network.Wai.Test

data H = H
mkYesod "H" [parseRoutes|
/ HeadR GET
|]
instance Yesod H

getHeadR :: Handler RepHtml
getHeadR = defaultLayout $ addScriptRemote "load.js"

specs :: [Spec]
specs = describe "Test.Links" [
    it "link from head" $ runner H $ do
      res <- request defaultRequest
      assertBody "<!DOCTYPE html>\n<html><head><title></title><script src=\"load.js\"></script></head><body></body></html>" res

  , it "link from head async" $ runner HA $ do
      res <- request defaultRequest
      assertBody "<!DOCTYPE html>\n<html><head><title></title><script src=\"yepnope.js\"></script><script>yepnope({load:[\"load.js\"]});</script></head><body></body></html>" res

  , it "link from bottom" $ runner B $ do
      res <- request defaultRequest
      assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><script src=\"load.js\"></script></body></html>" res
  ]

runner :: (YesodDispatch master master, Yesod master) => master -> Session () -> IO ()
runner app f = toWaiApp app >>= runSession f
