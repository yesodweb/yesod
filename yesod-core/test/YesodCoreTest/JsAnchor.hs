{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module YesodCoreTest.JsAnchor
  ( specs
  , Widget
  , resourcesJsAnchor
  ) where

import Test.Hspec

import Yesod.Core
import Network.Wai
import Network.Wai.Test

data JsAnchor = JsAnchor

mkYesod "JsAnchor" [parseRoutes|
/none NoneR GET
/one  OneR  GET
/two  TwoR  GET
|]

instance Yesod JsAnchor

getNoneR :: Handler Html
getNoneR = defaultLayout $ do
  toWidget [julius|window.$ = {};|]
  addScriptRemote "load.js"

getOneR :: Handler Html
getOneR = defaultLayout $ do
  toWidget [julius|window.$ = {};|]
  addScriptAnchor
  addScriptRemote "load.js"

getTwoR :: Handler Html
getTwoR = defaultLayout $ do
  toWidget [julius|window.$ = {};|]
  addScriptAnchor
  addScriptRemote "load.js"
  addScriptAnchor

runner :: Session () -> IO ()
runner f = toWaiApp JsAnchor >>= runSession f

specs :: Spec
specs = describe "Test.JsAnchor" $ do
  it "anchors to the end by default" $ runner $ do
    res <- request defaultRequest
      { pathInfo = ["none"] }
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><script src=\"load.js\"></script><script>window.$ = {};</script></body></html>" res
  it "anchors to the anchor if included" $ runner $ do
    res <- request defaultRequest
      { pathInfo = ["one"] }
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><script>window.$ = {};</script><script src=\"load.js\"></script></body></html>" res
  it "anchors to the first anchor if multiple included" $ runner $ do
    res <- request defaultRequest
      { pathInfo = ["two"] }
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><script>window.$ = {};</script><script src=\"load.js\"></script></body></html>" res

