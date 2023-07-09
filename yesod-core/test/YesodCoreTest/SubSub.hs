{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module YesodCoreTest.SubSub where

import Test.Hspec

import Yesod.Core
import Network.Wai.Test
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8

import YesodCoreTest.SubSubData

data App = App { getOuter :: OuterSubSite }

instance Yesod App

getSubR :: SubHandlerFor InnerSubSite master T.Text
getSubR = return $ T.pack "sub"

instance YesodSubDispatch OuterSubSite master where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesOuterSubSite)

instance YesodSubDispatch InnerSubSite master where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesInnerSubSite)

mkYesod "App" [parseRoutes|
/ OuterSubSiteR OuterSubSite getOuter
|]

app :: App
app = App { getOuter = OuterSubSite { getInner = InnerSubSite }}

runner :: Session () -> IO ()
runner f = toWaiApp app >>= runSession f

case_subSubsite :: IO ()
case_subSubsite = runner $ do
  res <- request defaultRequest
  assertBody (L8.pack "sub") res
  assertStatus 200 res

subSubTest :: Spec
subSubTest = describe "YesodCoreTest.SubSub" $ do
  it "sub_subsite" case_subSubsite