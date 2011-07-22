{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Links (linksTest) where

import Yesod.Core hiding (Request)
import Text.Hamlet

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Network.Wai
import Network.Wai.Test

import qualified Data.ByteString.Lazy.Char8 as L8

data Y = Y
mkYesod "Y" [$parseRoutes|
/ RootR GET
|]

instance Yesod Y where
    approot _ = ""

getRootR = defaultLayout $ addHamlet [$hamlet|<a href=@{RootR}>|]

linksTest :: Test
linksTest = testGroup "Test.Links"
    [ testCase "linkToHome" case_linkToHome
    ]

runner f = toWaiApp Y >>= runSession f
defaultRequest = Request
    { pathInfo = []
    , requestHeaders = []
    , queryString = []
    , requestMethod = "GET"
    }

case_linkToHome = runner $ do
    res <- request defaultRequest
    assertBody "<!DOCTYPE html>\n<html><head><title></title></head><body><a href=\"/\"></a></body></html>" res
