{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.JsAttributes
    ( specs
      -- To avoid unused warning
    , Widget
    , resourcesApp
    ) where

import Test.Hspec

import Yesod.Core
import Network.Wai.Test

data App = App
mkYesod "App" [parseRoutes|
/ HeadR GET
|]
instance Yesod App where
    jsAttributes _ = [("attr0", "a")]

getHeadR :: Handler Html
getHeadR = defaultLayout $ do
    addScriptRemote "load.js"
    toWidget [julius|/*body*/|]
    toWidgetHead [julius|/*head*/|]

specs :: Spec
specs = describe "Test.JsAttributes" $ do
    it "script in body gets attributes" $ runner App $ do
      res <- request defaultRequest
      assertBody "<!DOCTYPE html>\n<html><head><title></title><script>/*head*/</script></head><body><script src=\"load.js\"></script><script attr0=\"a\">/*body*/</script></body></html>" res

runner :: YesodDispatch master => master -> Session () -> IO ()
runner app f = toWaiApp app >>= runSession f

