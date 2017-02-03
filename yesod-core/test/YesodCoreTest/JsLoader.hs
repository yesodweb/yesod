{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module YesodCoreTest.JsLoader
    ( specs
    , Widget
    , resourcesH
    ) where

import YesodCoreTest.JsLoaderSites.Bottom (B(..))

import Test.Hspec

import Yesod.Core
import Network.Wai.Test

data H = H
mkYesod "H" [parseRoutes|
/ HeadR GET
|]
instance Yesod H where
    jsLoader _ = BottomOfHeadBlocking

getHeadR :: Handler Html
getHeadR = defaultLayout $ addScriptRemote "load.js"

specs :: Spec
specs = describe "Test.JsLoader" $ do
    it "link from head" $ runner H $ do
      res <- request defaultRequest
      assertBody "<!DOCTYPE html>\n<html><head><title></title><script src=\"load.js\"></script></head><body></body></html>" res

    it "link from bottom" $ runner B $ do
      res <- request defaultRequest
      assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><script src=\"load.js\"></script></body></html>" res

runner :: YesodDispatch master => master -> Session () -> IO ()
runner app f = toWaiApp app >>= runSession f
